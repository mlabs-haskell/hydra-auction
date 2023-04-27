module Main (main) where

-- Prelude imports
import Hydra.Prelude (lookupEnv, readMaybe, traverse_)
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
  writeTChan,
  writeTQueue,
 )
import Control.Monad (forever, void, when, (>=>))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Tracer (Tracer, contramap, nullTracer, stdoutTracer)
import Data.Aeson (ToJSON, eitherDecode, encode)
import Data.Maybe (fromMaybe)
import Network.WebSockets (
  Connection,
  acceptRequest,
  receiveData,
  runClient,
  runServer,
  sendTextData,
  withPingThread,
 )
import Prettyprinter (Pretty (pretty))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

-- Hydra imports
import Hydra.Network (IP, PortNumber)
import HydraNode (HydraClient (..))

-- HydraAuction imports

import GHC.Stack (HasCallStack)
import HydraAuction.Delegate (
  ClientId,
  ClientResponseScope (Broadcast),
  DelegateEvent (..),
  clientIsInScope,
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
import HydraAuction.Tx.Common (currentAuctionStage)
import HydraAuction.Types (AuctionTerms)
import HydraAuctionUtils.Composite.Runner (
  CompositeExecutionContext (..),
  CompositeRunner,
  executeCompositeRunner,
  runHydraInComposite,
 )
import HydraAuctionUtils.Fixture (Actor (..))
import HydraAuctionUtils.Hydra.Monad (AwaitedHydraEvent (..), waitForHydraEvent)
import HydraAuctionUtils.Hydra.Runner (HydraRunner, executeHydraRunnerFakingParams)
import HydraAuctionUtils.L1.Runner (dockerNode, executeRunnerWithNodeAs)
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
  forall void.
  -- | the time in milliseconds that the runner sleeps between acts
  Int ->
  -- | the queue of incoming messages
  TQueue DelegateEvent ->
  TQueue (ClientId, FrontendRequest) ->
  -- | the broadcast queue of outgoing messages (write only)
  TChan (ClientResponseScope, DelegateResponse) ->
  CompositeRunner void
-- FIXME: we need to abort at some point but this doesn't seem
-- to be implemented yet so we just go on
runDelegateLogicSteps
  tick
  eventQueue
  frontendRequestQueue
  broadcast = do
    flip evalStateT initialState $ forever $ do
      processQueueWith
        eventQueue
        ( delegateEventStep
            -- All delegateEventStep responses should be brodcasted
            >=> return . fmap (Broadcast,)
        )
      processQueueWith
        frontendRequestQueue
        delegateFrontendRequestStep
      -- FIXME: log queues overload and make tick not-static
      liftIO $ threadDelay tick
    where
      processQueueWith ::
        forall req.
        (Show req) =>
        TQueue req ->
        ( req ->
          StateT
            DelegateState
            CompositeRunner
            [(ClientResponseScope, DelegateResponse)]
        ) ->
        StateT DelegateState CompositeRunner ()
      processQueueWith queue step =
        flushAndTraverseNewElements queue $ \event -> do
          -- FIXME: use logging
          liftIO $ putStrLn $ "Delegate logic event: " <> show event
          response <- step event
          liftIO $ putStrLn $ "Delegate logic response: " <> show response
          putInToClientsQueue response
      putInToClientsQueue responses = liftIO $ do
        atomically $
          traverse_ (writeTChan broadcast) responses
      flushAndTraverseNewElements queue handler = do
        elements <- liftIO . atomically $ flushTQueue queue
        traverse_ handler elements

-- | run a delegate server
runDelegateServer ::
  -- | the configuration of the delegate server
  DelegateServerConfig ->
  DelegateTracerT IO ()
runDelegateServer conf = do
  trace (Started $ port conf)

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
              liftIO $
                runServer (show $ host conf) (fromIntegral $ port conf) $
                  runWithTracer' tracer
                    . websocketsServer
                      (ping conf)
                      (freshClientIdGenerator clientCounter)
                      frontendRequestQueue
                      toClientsChannel
            DelegateLogicStepsThread ->
              void $
                liftIO $ do
                  executeCompositeRunnerForConfig $
                    runDelegateLogicSteps
                      (tick conf)
                      eventQueue
                      frontendRequestQueue
                      toClientsChannel
            QueueAuctionStageThread ->
              mbQueueAuctionPhases eventQueue toClientsChannel
            QueueHydraEventsThread ->
              liftIO $
                executeCompositeRunnerForConfig . runHydraInComposite $
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
    executeCompositeRunnerForConfig action =
      runHydraClientN (hydraServerNumber conf) $ \hydraClient -> do
        context <- executeRunnerWithNodeAs dockerNode Alice $ do
          l1Context <- ask
          executeHydraRunnerFakingParams hydraClient $ do
            hydraContext <- ask
            return $
              MkCompositeExecutionContext {hydraContext, l1Context}
        executeCompositeRunner context action

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
  TQueue DelegateEvent ->
  TChan (ClientResponseScope, DelegateResponse) ->
  DelegateTracerT IO ()
mbQueueAuctionPhases delegateEvents toClientsChannel = do
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
          -- TODO: use tick
          threadDelay 1000
          awaitForAuctionSetTerms chan

    queueCurrentStageAndWaitForNextLoop :: AuctionTerms -> IO ()
    queueCurrentStageAndWaitForNextLoop terms = do
      currentStage <- currentAuctionStage terms
      atomically $
        writeTQueue delegateEvents $
          AuctionStageStarted currentStage
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
          <> "until next stage."
      case mSecsLeft of
        Nothing -> pure ()
        Just s -> do
          threadDelay (fromInteger s * 1_000_000)
          queueCurrentStageAndWaitForNextLoop terms

{- | start a delegate server at a @$PORT@,
   it accepts incoming websocket connections
   and runs the delegate.
-}
main :: HasCallStack => IO ()
main = do
  --- XXX: this prevents logging issues in Docker
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  port <- lookupEnv "PORT"
  -- FIXUP: parse actual adress and other params
  envHydraNode <- lookupEnv "HYDRA_NODE_NUMBER"
  hydraNodeNumber <-
    maybe
      (fail "could not find HYDRA_NODE_NUMBER environment variable")
      pure
      $ readMaybe =<< envHydraNode

  putStrLn $ "With number: " <> show hydraNodeNumber
  let actor = case hydraNodeNumber of
        1 -> Oscar
        2 -> Patricia
        3 -> Rupert
        _ -> error "Wrong HYDRA_NODE_NUMBER"
  let conf :: DelegateServerConfig
      conf =
        DelegateServerConfig
          { host
          , port = fromMaybe portDefault $ port >>= readMaybe
          , tick = tick
          , ping = ping
          , l1Actor = actor
          , hydraServerNumber = hydraNodeNumber
          }
  runWithTracer' tracer $ runDelegateServer conf
  where
    tracer :: Tracer IO DelegateServerLog
    tracer = contramap (show . pretty) stdoutTracer

    host :: IP
    host = "0.0.0.0"

    tick :: Int
    tick = 1_000

    ping :: Int
    ping = 30

    portDefault :: PortNumber
    portDefault = 8001

runHydraClientN :: Int -> (HydraClient -> IO b) -> IO b
runHydraClientN n cont' = do
  putStrLn $ "Hydra connection opened for " <> show n
  runClient ("172.16.238." <> show (10 * n)) 4001 "/history=yes" $
    \connection ->
      cont' $
        HydraClient
          { hydraNodeId = n
          , connection = connection
          , tracer = contramap show nullTracer
          }
