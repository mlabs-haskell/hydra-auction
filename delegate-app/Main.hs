module Main (main) where

-- Prelude imports
import Hydra.Cardano.Api.Prelude (lookupEnv)
import Hydra.Prelude (readMaybe, traverse_)
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
import Control.Monad.Trans (MonadIO (liftIO), MonadTrans (lift))
import Control.Tracer (Tracer, contramap, stdoutTracer)
import Data.Aeson (ToJSON, eitherDecode, encode)
import Data.Maybe (fromMaybe)
import Network.WebSockets (
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
  DelegateEvent (AuctionStageStarted, Start),
  DelegateInput (DelegateEvent, FrontendRequest),
  DelegateRunnerT,
  delegateStep,
  execDelegateRunnerT,
 )
import HydraAuction.Delegate.Interface (DelegateResponse (AuctionSet), FrontendRequest)
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
    DelegateOutput,
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
delegateServerApp ::
  -- | the amount of seconds between pings
  Int ->
  -- | the queue for the inputs of the delegates (this is where the requests are enqueued)
  TQueue DelegateInput ->
  -- | the queue for the broadcast (this is where we receives events from the runner)
  TChan DelegateResponse ->
  ServerAppT (DelegateTracerT IO)
delegateServerApp pingSecs reqq broadcast pending = do
  connection <- liftIO $ acceptRequest pending

  trace FrontendConnected
  tracer <- askTracer

  liftIO $ do
    broadcast' <- atomically $ dupTChan broadcast

    let encodeSend :: forall a. ToJSON a => a -> IO ()
        encodeSend = sendTextData connection . encode

        receive :: forall void. IO void
        receive = forever $
          runWithTracer' tracer $ do
            inp <- liftIO $ receiveData connection
            case eitherDecode @FrontendRequest inp of
              Left msg -> do
                let err = FrontendNoParse msg
                trace (DelegateError err)
                liftIO $ encodeSend err
              Right req -> do
                trace $ FrontendInput req
                liftIO . atomically . writeTQueue reqq $ FrontendRequest req

        send :: forall void. IO void
        send =
          forever $
            atomically (readTChan broadcast')
              >>= encodeSend

    -- From the Documentation of 'withPingThread' in 'Network.Websockets':
    -- This is useful to keep idle connections open through proxies and whatnot.
    -- Many (but not all) proxies have a 60 second default timeout, so based on
    -- that sending a ping every 30 seconds is a good idea.
    withPingThread connection pingSecs (pure ()) $ race_ receive send

{- | create a delegate runner that will listen for incoming events and output outgoing events
   accordingly
-}
mkRunner ::
  forall void.
  -- | the time in milliseconds that the runner sleeps between acts
  Int ->
  -- | the queue of incoming messages
  TQueue DelegateInput ->
  -- | the broadcast queue of outgoing messages (write only)
  TChan DelegateResponse ->
  DelegateRunnerT (DelegateTracerT IO) void
-- FIXME: we need to abort at some point but this doesn't seem
-- to be implemented yet so we just go on
mkRunner tick reqq broadcast = forever $ do
  let encodeBroadcast responses = do
        liftIO . atomically $ traverse_ (writeTChan broadcast) responses
        lift $ traverse_ (trace . DelegateOutput) responses

  events <- liftIO . atomically $ flushTQueue reqq
  traverse_ (delegateStep >=> encodeBroadcast) events

  liftIO $ threadDelay tick

-- | run a delegate server
runDelegateServer ::
  -- | the configuration of the delegate server
  DelegateServerConfig ->
  DelegateTracerT IO ()
runDelegateServer conf = do
  trace (Started $ port conf)

  tracer <- askTracer

  delegateInputChan <- liftIO . atomically $ do
    q <- newTQueue
    writeTQueue q (DelegateEvent Start)
    pure q
  delegateBroadCastChan <- liftIO . atomically $ newTChan

  let wsServer, delegateRunner, queueAuctionPhases :: IO ()
      wsServer =
        runServer (show $ host conf) (fromIntegral $ port conf) $
          runWithTracer' tracer . delegateServerApp (ping conf) delegateInputChan delegateBroadCastChan

      delegateRunner =
        runWithTracer' tracer . execDelegateRunnerT $ mkRunner (tick conf) delegateInputChan delegateBroadCastChan

      queueAuctionPhases = runWithTracer' tracer $ mbQueueAuctionPhases delegateInputChan delegateBroadCastChan

  liftIO $
    mapConcurrently_
      (runWithTracer' tracer . uncurry startWorker)
      [ (WebsocketThread, wsServer)
      , (QueueAuctionStageThread, queueAuctionPhases)
      , (DelegateRunnerThread, delegateRunner)
      ]

-- | start a worker and log it
startWorker :: ThreadSort -> IO () -> DelegateTracerT IO ()
startWorker thread act = do
  trace $ ThreadEvent ThreadStarted thread
  liftIO act
  trace $ ThreadEvent ThreadCancelled thread

mbQueueAuctionPhases :: TQueue DelegateInput -> TChan DelegateResponse -> DelegateTracerT IO ()
mbQueueAuctionPhases reqq broadcast = do
  terms <- liftIO . atomically $ getTerms =<< dupTChan broadcast
  liftIO $ queueStages terms
  traceQueueEvent terms
  where
    traceQueueEvent :: AuctionTerms -> DelegateTracerT IO ()
    traceQueueEvent = trace . QueueAuctionPhaseEvent . ReceivedAuctionSet

    getTerms :: TChan DelegateResponse -> STM AuctionTerms
    getTerms chan =
      readTChan chan >>= \case
        AuctionSet terms -> pure terms
        _ -> getTerms chan

    queueStages :: AuctionTerms -> IO ()
    queueStages terms = do
      currentStage <- currentAuctionStage terms
      atomically $ writeTQueue reqq (DelegateEvent $ AuctionStageStarted currentStage)
      currentPosixTime <- POSIXTime <$> currentTimeMilliseconds
      let mSecsLeft = secondsLeftInInterval currentPosixTime (stageToInterval terms currentStage)
      case mSecsLeft of
        Nothing -> pure ()
        Just s -> do
          threadDelay (fromInteger s * 1000)
          queueStages terms

{- | start a delegate server at a @$PORT@, it accepts incoming websocket connections and
   runs the delegate
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
