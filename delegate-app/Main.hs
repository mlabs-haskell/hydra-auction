module Main (main) where

-- Prelude imports
import Hydra.Prelude (traverse_)
import Prelude

-- Haskell imports

import Control.Concurrent (newMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (mapConcurrently_, race_)
import Control.Concurrent.STM (
  TChan,
  TQueue,
  atomically,
  dupTChan,
  flushTQueue,
  newTChan,
  newTQueue,
  readTChan,
  tryReadTQueue,
  writeTChan,
  writeTQueue,
 )
import Control.Monad (forever, void, when, (>=>))
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Tracer (Tracer, contramap, stdoutTracer)
import Data.Aeson (ToJSON, eitherDecode, encode)
import Network.WebSockets (
  Connection,
  acceptRequest,
  receiveData,
  runServer,
  sendTextData,
  withPingThread,
 )
import Prettyprinter (Pretty (pretty))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

-- Hydra imports
import Hydra.Network (Host (..))

-- HydraAuction imports

import Data.Text qualified as Text
import DelegateServer.Parsers (delegateConfigParser)
import GHC.Stack (HasCallStack)
import HydraAuction.Delegate (
  DelegateEvent (..),
  delegateEventStep,
  delegateFrontendRequestStep,
 )
import HydraAuction.Delegate.Interface (
  DelegateResponse (AuctionSet),
  DelegateState,
  FrontendRequest,
  initialState,
 )
import HydraAuction.Delegate.Server (
  DelegateServerConfig (..),
  DelegateServerLog (..),
  DelegateTracerT,
  QueueAuctionPhaseEvent (ReceivedAuctionSet),
  ServerAppT,
  ThreadEvent (ThreadCancelled, ThreadStarted),
  ThreadSort (..),
 )
import HydraAuction.OnChain.Common (secondsLeftInInterval, stageToInterval)
import HydraAuction.Platform.Interface (PlatformProtocol)
import HydraAuction.Tx.Common (currentAuctionStage)
import HydraAuction.Types (AuctionTerms)
import HydraAuctionUtils.Composite.Runner (
  CompositeRunner,
 )
import HydraAuctionUtils.Hydra.Interface (
  HydraConnectionConfig (..),
 )
import HydraAuctionUtils.Hydra.Monad (waitForHydraEvent)
import HydraAuctionUtils.Hydra.Runner (HydraRunner, executeHydraRunnerFakingParams)
import HydraAuctionUtils.L1.Runner (executeL1RunnerWithNodeAs)
import HydraAuctionUtils.Parsers (execParserForCliArgs)
import HydraAuctionUtils.Server.Client (
  AwaitedOutput (..),
  withProtocolClient,
 )
import HydraAuctionUtils.Server.ClientId (
  ClientId,
  ClientResponseScope (Broadcast),
  clientIsInScope,
 )
import HydraAuctionUtils.Server.Protocol (ProtocolClientFor, withClient)
import HydraAuctionUtils.Time (currentPlutusPOSIXTime)
import HydraAuctionUtils.Tracing (
  MonadTracer (trace),
  askTracer,
  runWithTracer',
 )

{- | per websocket, a thread gets spawned which then
   subscribes to the broadcast channel and sends to
   the incoming channel of the delegate runner
-}
websocketsServer ::
  -- | the amount of seconds between pings
  Int ->
  IO ClientId ->
  -- | the queue for the frontend requests of the delegates
  --   (this is where the requests are enqueued)
  TQueue (ClientId, FrontendRequest) ->
  -- | the queue for the broadcast (this is where we receives events from the runner)
  TChan (ClientResponseScope, DelegateResponse) ->
  ServerAppT (DelegateTracerT IO)
websocketsServer
  pingSecs
  genFreshClientId
  frontendRequestQueue
  broadcast
  pending = do
    connection <- liftIO $ acceptRequest pending
    clientId <- liftIO genFreshClientId

    trace $ GotFrontendConnected clientId
    tracer <- askTracer

    liftIO $ do
      toClientsChannelCopy <- atomically $ dupTChan broadcast
      -- From the Documentation of 'withPingThread' in 'Network.Websockets':
      -- This is useful to keep idle connections open through proxies and whatnot.
      -- Many (but not all) proxies have a 60 second default timeout, so based on
      -- that sending a ping every 30 seconds is a good idea.
      withPingThread connection pingSecs (pure ()) $
        race_
          (forever $ receiveAndPutInQueue tracer connection clientId)
          ( forever $
              sendFromChannel connection clientId toClientsChannelCopy
          )
    where
      sendToClient :: forall a. ToJSON a => Connection -> a -> IO ()
      sendToClient connection = sendTextData connection . encode
      receiveAndPutInQueue ::
        Tracer IO DelegateServerLog ->
        Connection ->
        ClientId ->
        IO ()
      receiveAndPutInQueue tracer connection clientId =
        runWithTracer' tracer $ do
          inp <- liftIO $ receiveData connection
          case eitherDecode @FrontendRequest inp of
            Left _ -> return ()
            Right request -> do
              trace $ GotFrontendRequest request
              liftIO . atomically $
                writeTQueue frontendRequestQueue (clientId, request)
      sendFromChannel ::
        Connection ->
        ClientId ->
        TChan (ClientResponseScope, DelegateResponse) ->
        IO ()
      sendFromChannel connection clientId broadcastCopy = do
        (scope, message) <- atomically (readTChan broadcastCopy)
        when (clientIsInScope clientId scope) $
          sendToClient connection message

{- | create a delegate runner that will listen for incoming events
     and output outgoing events accordingly
-}
runDelegateLogicSteps ::
  forall void client.
  ProtocolClientFor PlatformProtocol client =>
  -- | the time in milliseconds that the runner sleeps between acts
  Int ->
  client ->
  -- | the queue of incoming messages
  TQueue DelegateEvent ->
  TQueue (ClientId, FrontendRequest) ->
  -- | the broadcast queue of outgoing messages (write only)
  TChan (ClientResponseScope, DelegateResponse) ->
  CompositeRunner void
runDelegateLogicSteps
  tick
  platformClient
  eventQueue
  frontendRequestQueue
  broadcast = do
    flip evalStateT initialState $ forever $ do
      -- Any Hydra state changes could affect FrontendRequest execution
      -- That means that we cannot process multiple FrontendRequests
      -- on same run, cuz interleaving Hydra events might affect them.
      -- To prevent that we first process all Hydra events in queue,
      -- and then process not more than one FrontendRequest in a tick.

      processQueueWith
        eventQueue
        ( (withClient platformClient . delegateEventStep)
            -- All delegateEventStep responses should be brodcasted
            >=> return . fmap (Broadcast,)
        )

      mEvent <- liftIO . atomically $ tryReadTQueue frontendRequestQueue
      traverse_
        (performStep delegateFrontendRequestStep)
        mEvent

      -- FIXME: log queues overload and make tick not-static
      liftIO $ threadDelay tick
    where
      performStep ::
        forall req.
        (Show req) =>
        ( req ->
          StateT
            DelegateState
            CompositeRunner
            [(ClientResponseScope, DelegateResponse)]
        ) ->
        req ->
        StateT DelegateState CompositeRunner ()
      performStep step event = do
        -- FIXME: use logging
        liftIO $ putStrLn $ "Delegate logic event: " <> show event
        response <- step event
        liftIO $ putStrLn $ "Delegate logic response: " <> show response
        putInToClientsQueue response
      processQueueWith queue step =
        flushAndTraverseNewElements queue $ performStep step
      putInToClientsQueue responses = liftIO $ do
        atomically $
          traverse_ (writeTChan broadcast) responses
      flushAndTraverseNewElements queue handler = do
        elements <- liftIO . atomically $ flushTQueue queue
        traverse_ handler elements

-- | run a delegate server
runDelegateServer ::
  HasCallStack =>
  -- | the configuration of the delegate server
  DelegateServerConfig ->
  DelegateTracerT IO ()
runDelegateServer conf = do
  tracer <- askTracer

  eventQueue <- liftIO . atomically $ do
    q <- newTQueue
    writeTQueue q Start
    pure q
  frontendRequestQueue <- liftIO . atomically $ newTQueue
  toClientsChannel <- liftIO . atomically $ newTChan

  clientCounter <- liftIO $ newMVar 0

  let workerAction threadSort =
        withStartStopTrace threadSort $
          case threadSort of
            WebsocketThread -> do
              let host = websocketsHost conf
              liftIO $
                runServer (Text.unpack $ hostname host) (fromIntegral $ port host) $
                  runWithTracer' tracer
                    . websocketsServer
                      (ping conf)
                      (freshClientIdGenerator clientCounter)
                      frontendRequestQueue
                      toClientsChannel
            DelegateLogicStepsThread ->
              void $
                liftIO $ do
                  withProtocolClient (platformHost conf) () $ \platformClient ->
                    executeHydraRunnerForConfig $
                      runDelegateLogicSteps
                        (tick conf)
                        platformClient
                        eventQueue
                        frontendRequestQueue
                        toClientsChannel
            QueueAuctionStageThread ->
              mbQueueAuctionPhases (tick conf) eventQueue toClientsChannel
            QueueHydraEventsThread ->
              liftIO $
                executeHydraRunnerForConfig $
                  queueHydraEvents eventQueue

  liftIO $
    mapConcurrently_
      (runWithTracer' tracer . workerAction)
      [ WebsocketThread
      , QueueAuctionStageThread
      , QueueHydraEventsThread
      , DelegateLogicStepsThread
      ]
  where
    freshClientIdGenerator clientCounter = do
      v <- takeMVar clientCounter
      putMVar clientCounter (v + 1)
      return v
    executeHydraRunnerForConfig action =
      withProtocolClient (hydraNodeHost conf) clientConfig $ \hydraClient -> do
        executeL1RunnerWithNodeAs (cardanoNode conf) (l1Actor conf) $ do
          executeHydraRunnerFakingParams hydraClient action
    clientConfig = MkHydraConnectionConfig {retrieveHistory = True}

queueHydraEvents ::
  forall void. TQueue DelegateEvent -> HydraRunner void
queueHydraEvents delegateEventQueue = forever $ do
  mEvent <- waitForHydraEvent Any
  case mEvent of
    Just event ->
      liftIO $
        atomically $
          writeTQueue delegateEventQueue $
            HydraEvent event
    Nothing ->
      liftIO $ putStrLn "Not recieved Hydra events for waiting time"

-- | start a worker and log it
withStartStopTrace :: ThreadSort -> DelegateTracerT IO () -> DelegateTracerT IO ()
withStartStopTrace threadSort action = do
  trace $ ThreadEvent ThreadStarted threadSort
  action
  -- FIXME: use finally or something like that
  trace $ ThreadEvent ThreadCancelled threadSort

mbQueueAuctionPhases ::
  Int ->
  TQueue DelegateEvent ->
  TChan (ClientResponseScope, DelegateResponse) ->
  DelegateTracerT IO ()
mbQueueAuctionPhases tick delegateEvents toClientsChannel = do
  terms <-
    liftIO $
      awaitForAuctionSetTerms =<< atomically (dupTChan toClientsChannel)
  traceQueueEvent terms
  liftIO $ queueCurrentStageAndWaitForNextLoop terms
  where
    traceQueueEvent :: AuctionTerms -> DelegateTracerT IO ()
    traceQueueEvent = trace . QueueAuctionPhaseEvent . ReceivedAuctionSet

    awaitForAuctionSetTerms ::
      TChan (ClientResponseScope, DelegateResponse) -> IO AuctionTerms
    awaitForAuctionSetTerms chan =
      atomically (readTChan chan) >>= \case
        (Broadcast, AuctionSet terms) -> pure terms
        _ -> do
          threadDelay tick
          awaitForAuctionSetTerms chan

    queueCurrentStageAndWaitForNextLoop :: AuctionTerms -> IO ()
    queueCurrentStageAndWaitForNextLoop terms = do
      currentStage <- currentAuctionStage terms
      atomically $
        writeTQueue delegateEvents $
          AuctionStageStarted terms currentStage
      currentPosixTime <- currentPlutusPOSIXTime
      let mSecsLeft =
            secondsLeftInInterval
              currentPosixTime
              (stageToInterval terms currentStage)
      putStrLn $
        "Current stage is "
          <> show currentStage
          <> " , awaiting "
          <> show mSecsLeft
          <> " until next stage."
      case mSecsLeft of
        Nothing -> pure ()
        Just s -> do
          -- In case of on stage intervals boundaries,
          -- `secsToWait` is 0, which leads to stuttering
          let secsToWait = if s == 0 then 1 else s
          threadDelay $ fromInteger $ secsToWait * 1_000_000
          queueCurrentStageAndWaitForNextLoop terms

main :: HasCallStack => IO ()
main = do
  --- XXX: this prevents logging issues in Docker
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  conf <- execParserForCliArgs delegateConfigParser
  runWithTracer' tracer $ do
    trace $ Started conf
    runDelegateServer conf
  where
    tracer :: Tracer IO DelegateServerLog
    tracer = contramap (show . pretty) stdoutTracer
