module Main (main) where

-- Prelude imports
import Hydra.Prelude (lookupEnv, readMaybe, traverse_)
import Prelude

-- Haskell imports
import Control.Concurrent (newMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (mapConcurrently_, race_)
import Control.Concurrent.STM (
  STM,
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
import Control.Monad.State (runStateT)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Tracer (Tracer, contramap, stdoutTracer)
import Data.Aeson (ToJSON, eitherDecode, encode)
import Data.Maybe (fromMaybe)
import Network.WebSockets (
  Connection,
  acceptRequest,
  receiveData,
  runServer,
  sendTextData,
  withPingThread,
 )
import Prettyprinter (Pretty (pretty))

-- Hydra imports
import Hydra.Network (IP, PortNumber)

-- Plutus imports
import Plutus.V2.Ledger.Api (POSIXTime (..))

-- HydraAuction imports

import HydraAuction.Delegate (
  ClientId,
  ClientResponseScope (Broadcast),
  DelegateEvent (..),
  clientIsInScope,
  delegateEventStep,
  delegateFrontendRequestStep,
 )
import HydraAuction.Delegate.CompositeRunner (CompositeRunner, executeCompositeRunner)
import HydraAuction.Delegate.Interface (
  DelegateResponse (AuctionSet),
  FrontendRequest,
  initialState,
 )
import HydraAuction.Delegate.Server (
  DelegateError (FrontendNoParse),
  DelegateServerConfig (
    DelegateServerConfig,
    host,
    ping,
    port,
    tick
  ),
  DelegateServerLog (
    DelegateError,
    FrontendConnected,
    FrontendInput,
    QueueAuctionPhaseEvent,
    Started,
    ThreadEvent
  ),
  DelegateTracerT,
  QueueAuctionPhaseEvent (ReceivedAuctionSet),
  ServerAppT,
  ThreadEvent (ThreadCancelled, ThreadStarted),
  ThreadSort (DelegateLogicStepsThread, QueueAuctionStageThread, WebsocketThread),
 )
import HydraAuction.OnChain.Common (secondsLeftInInterval, stageToInterval)
import HydraAuction.Tx.Common (currentAuctionStage, currentTimeMilliseconds)
import HydraAuction.Types (AuctionTerms)
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

    trace $ FrontendConnected clientId
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
            Left msg -> do
              let err = FrontendNoParse msg
              trace (DelegateError err)
              liftIO $ sendToClient connection err
            Right request -> do
              trace $ FrontendInput request
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
    (voidResult, _) <-
      flip runStateT initialState $ forever $ do
        flushAndTraverseNewElements
          eventQueue
          ( delegateEventStep
              -- All delegateEventStep responses should be brodcasted
              >=> return . fmap (Broadcast,)
              >=> putInToClientsQueue
          )
        flushAndTraverseNewElements
          frontendRequestQueue
          ( delegateFrontendRequestStep
              >=> putInToClientsQueue
          )
        -- FIXME: log queues overload and make tick not-static
        liftIO $ threadDelay tick
    return voidResult
    where
      -- TODO: this is not actually broadcast now
      putInToClientsQueue responses = do
        liftIO . atomically $
          traverse_ (writeTChan broadcast) responses
      -- TODO: make work
      -- lift $ traverse_ (trace . DelegateOutput) responses
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
                liftIO $
                  executeCompositeRunner (error "TODO") $
                    runDelegateLogicSteps
                      (tick conf)
                      eventQueue
                      frontendRequestQueue
                      toClientsChannel
            QueueAuctionStageThread ->
              mbQueueAuctionPhases eventQueue toClientsChannel

  liftIO $
    mapConcurrently_
      (runWithTracer' tracer . workerAction)
      [ WebsocketThread
      , QueueAuctionStageThread
      , DelegateLogicStepsThread
      ]
  where
    freshClientIdGenerator clientCounter = do
      v <- takeMVar clientCounter
      putMVar clientCounter (v + 1)
      return v

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
    liftIO . atomically $
      awaitForAuctionSetTerms =<< dupTChan toClientsChannel
  traceQueueEvent terms
  liftIO $ queueCurrentStageAndWaitForNextLoop terms
  where
    traceQueueEvent :: AuctionTerms -> DelegateTracerT IO ()
    traceQueueEvent = trace . QueueAuctionPhaseEvent . ReceivedAuctionSet

    awaitForAuctionSetTerms ::
      TChan (ClientResponseScope, DelegateResponse) -> STM AuctionTerms
    awaitForAuctionSetTerms chan =
      readTChan chan >>= \case
        (Broadcast, AuctionSet terms) -> pure terms
        -- FIXME: maybe thread wait?
        _ -> awaitForAuctionSetTerms chan

    queueCurrentStageAndWaitForNextLoop :: AuctionTerms -> IO ()
    queueCurrentStageAndWaitForNextLoop terms = do
      currentStage <- currentAuctionStage terms
      atomically $
        writeTQueue delegateEvents $
          AuctionStageStarted currentStage
      currentPosixTime <- POSIXTime <$> currentTimeMilliseconds
      let mSecsLeft =
            secondsLeftInInterval
              currentPosixTime
              (stageToInterval terms currentStage)
      case mSecsLeft of
        Nothing -> pure ()
        Just s -> do
          threadDelay (fromInteger s * 1000)
          queueCurrentStageAndWaitForNextLoop terms

{- | start a delegate server at a @$PORT@,
   it accepts incoming websocket connections
   and runs the delegate.
-}
main :: IO ()
main = do
  port <- lookupEnv "PORT"

  let conf :: DelegateServerConfig
      conf =
        DelegateServerConfig
          { host
          , port = fromMaybe portDefault $ port >>= readMaybe
          , tick = tick
          , ping = ping
          }
  runWithTracer' tracer $ runDelegateServer conf
  where
    tracer :: Tracer IO DelegateServerLog
    tracer = contramap (show . pretty) stdoutTracer

    host :: IP
    host = "127.0.0.1"

    tick :: Int
    tick = 1_000

    ping :: Int
    ping = 30

    portDefault :: PortNumber
    portDefault = 8080
