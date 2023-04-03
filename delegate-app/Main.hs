module Main (main) where

-- Prelude imports
import Hydra.Prelude (lookupEnv, readMaybe, traverse_)
import Prelude

-- Haskell imports
import Control.Concurrent (threadDelay)
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
import Control.Monad (forever, (>=>))
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
-- Hydra imports
import Hydra.Network (IP, PortNumber)

-- Plutus imports
import Plutus.V2.Ledger.Api (POSIXTime (..))

-- HydraAuction imports
import HydraAuction.Delegate (
  DelegateEvent (..),
  DelegateRunnerT,
  delegateEventStep,
  delegateFrontendRequestStep,
  execDelegateRunnerT,
 )
import HydraAuction.Delegate.Interface (
  DelegateResponse (AuctionSet),
  FrontendRequest,
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
  ThreadSort (DelegateRunnerThread, QueueAuctionStageThread, WebsocketThread),
 )
import HydraAuction.OnChain.Common (secondsLeftInInterval, stageToInterval)
import HydraAuction.Tx.Common (currentAuctionStage, currentTimeMilliseconds)
import HydraAuction.Types (AuctionTerms)
import HydraAuctionUtils.Tracing (MonadTracer (trace), askTracer, runWithTracer')

{- | per websocket, a thread gets spawned which then
   subscribes to the broadcast channel and sends to
   the incoming channel of the delegate runner
-}
websocketsServer ::
  -- | the amount of seconds between pings
  Int ->
  -- | the queue for the frontend requests of the delegates
  --   (this is where the requests are enqueued)
  TQueue FrontendRequest ->
  -- | the queue for the broadcast (this is where we receives events from the runner)
  TChan DelegateResponse ->
  ServerAppT (DelegateTracerT IO)
websocketsServer pingSecs frontendRequestQueue broadcast pending = do
  connection <- liftIO $ acceptRequest pending

  trace FrontendConnected
  tracer <- askTracer

  liftIO $ do
    broadcastChannelCopy <- atomically $ dupTChan broadcast
    -- From the Documentation of 'withPingThread' in 'Network.Websockets':
    -- This is useful to keep idle connections open through proxies and whatnot.
    -- Many (but not all) proxies have a 60 second default timeout, so based on
    -- that sending a ping every 30 seconds is a good idea.
    withPingThread connection pingSecs (pure ()) $
      race_
        (forever $ receiveAndPutInQueue tracer connection)
        (forever $ sendFromChannel connection broadcastChannelCopy)
  where
    sendToClient :: forall a. ToJSON a => Connection -> a -> IO ()
    sendToClient connection = sendTextData connection . encode
    receiveAndPutInQueue ::
      Tracer IO DelegateServerLog ->
      Connection ->
      IO ()
    receiveAndPutInQueue tracer connection =
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
              writeTQueue frontendRequestQueue request
    sendFromChannel :: Connection -> TChan DelegateResponse -> IO ()
    sendFromChannel connection broadcastCopy =
      atomically (readTChan broadcastCopy)
        >>= sendToClient connection

{- | create a delegate runner that will listen for incoming events
     and output outgoing events accordingly
-}
runDelegateLogicSteps ::
  forall void.
  -- | the time in milliseconds that the runner sleeps between acts
  Int ->
  -- | the queue of incoming messages
  TQueue DelegateEvent ->
  TQueue FrontendRequest ->
  -- | the broadcast queue of outgoing messages (write only)
  TChan DelegateResponse ->
  DelegateRunnerT (DelegateTracerT IO) void
-- FIXME: we need to abort at some point but this doesn't seem
-- to be implemented yet so we just go on
runDelegateLogicSteps
  tick
  eventQueue
  frontendRequestQueue
  broadcast = forever $ do
    flushAndTraverseNewElements
      eventQueue
      (delegateEventStep >=> encodeAndBroadcast)
    flushAndTraverseNewElements
      frontendRequestQueue
      ( (\request -> return (0, request))
          >=> delegateFrontendRequestStep
          >=> return . fmap snd
          >=> encodeAndBroadcast
      )
    -- FIXME: log queues overload and make tick not-static
    liftIO $ threadDelay tick
    where
      -- TODO: this is not actually broadcast now
      encodeAndBroadcast responses = do
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
  broadcastChannel <- liftIO . atomically $ newTChan

  let workerAction threadSort =
        withStartStopTrace threadSort $
          case threadSort of
            WebsocketThread -> do
              liftIO $
                runServer (show $ host conf) (fromIntegral $ port conf) $
                  runWithTracer' tracer
                    . websocketsServer
                      (ping conf)
                      frontendRequestQueue
                      broadcastChannel
            DelegateRunnerThread ->
              execDelegateRunnerT $
                runDelegateLogicSteps
                  (tick conf)
                  eventQueue
                  frontendRequestQueue
                  broadcastChannel
            QueueAuctionStageThread ->
              mbQueueAuctionPhases eventQueue broadcastChannel

  liftIO $
    mapConcurrently_
      (runWithTracer' tracer . workerAction)
      [ WebsocketThread
      , QueueAuctionStageThread
      , DelegateRunnerThread
      ]

-- | start a worker and log it
withStartStopTrace :: ThreadSort -> DelegateTracerT IO () -> DelegateTracerT IO ()
withStartStopTrace threadSort action = do
  trace $ ThreadEvent ThreadStarted threadSort
  action
  -- FIXME: use finally or something like that
  trace $ ThreadEvent ThreadCancelled threadSort

mbQueueAuctionPhases :: TQueue DelegateEvent -> TChan DelegateResponse -> DelegateTracerT IO ()
mbQueueAuctionPhases delegateEvents broadcast = do
  terms <-
    liftIO . atomically $
      awaitForAuctionSetTerms =<< dupTChan broadcast
  traceQueueEvent terms
  liftIO $ forever $ queueCurrentStageAndWaitForNext terms
  where
    traceQueueEvent :: AuctionTerms -> DelegateTracerT IO ()
    traceQueueEvent = trace . QueueAuctionPhaseEvent . ReceivedAuctionSet

    awaitForAuctionSetTerms :: TChan DelegateResponse -> STM AuctionTerms
    awaitForAuctionSetTerms chan =
      readTChan chan >>= \case
        AuctionSet terms -> pure terms
        -- FIXME: maybe thread wait?
        _ -> awaitForAuctionSetTerms chan

    queueCurrentStageAndWaitForNext :: AuctionTerms -> IO ()
    queueCurrentStageAndWaitForNext terms = do
      currentStage <- currentAuctionStage terms
      atomically $ writeTQueue delegateEvents $ AuctionStageStarted currentStage
      currentPosixTime <- POSIXTime <$> currentTimeMilliseconds
      let mSecsLeft = secondsLeftInInterval currentPosixTime (stageToInterval terms currentStage)
      case mSecsLeft of
        Nothing -> pure ()
        Just s -> do
          threadDelay (fromInteger s * 1000)

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
