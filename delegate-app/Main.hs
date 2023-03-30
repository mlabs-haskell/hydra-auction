module Main (main) where

-- Prelude imports
import Hydra.Cardano.Api.Prelude (lookupEnv)
import Hydra.Prelude (race_, readMaybe, traverse_)
import Prelude

-- Haskell imports
import Control.Concurrent (forkIO, threadDelay)
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
import Control.Monad (forever, (>=>))
import Control.Monad.Trans (MonadIO (..), MonadTrans (lift))
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
import Hydra.Network (IP)

-- HydraAuction imports
import HydraAuction.Delegate (
  DelegateEvent (Start),
  DelegateInput (DelegateEvent, FrontendRequest),
  DelegateRunnerT,
  delegateStep,
  execDelegateRunnerT,
 )
import HydraAuction.Delegate.Interface (DelegateResponse, FrontendRequest)
import HydraAuction.Delegate.Server (
  DelegateError (FrontendNoParse),
  DelegateServerConfig (
    DelegateServerConfig,
    dlgt'host,
    dlgt'ping,
    dlgt'port,
    dlgt'tick
  ),
  DelegateServerLog (
    DelegateError,
    DelegateOutput,
    FrontendConnected,
    FrontendInput,
    Started
  ),
  DelegateTracerT,
  ServerAppT,
 )
import HydraAuction.Delegate.Tracing (MonadTracer (trace), TracerT, askTracer, runWithTracer')

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

        send =
          forever $
            atomically (readTChan broadcast')
              >>= encodeSend

    withPingThread connection pingSecs (pure ()) $ race_ receive send -- TODO: can we do this

{- | create a delegate runner that will listen for incoming events and output outgoing events
   accordingly
-}
mkRunner ::
  -- | the time in milliseconds that the runner sleeps between acts
  Int ->
  -- | the queue of incoming messages
  TQueue DelegateInput ->
  -- | the broadcast queue of outgoing messages (write only)
  TChan DelegateResponse ->
  DelegateRunnerT (DelegateTracerT IO) ()
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
  TracerT DelegateServerLog IO ()
runDelegateServer conf = do
  trace (Started $ dlgt'port conf)

  tracer <- askTracer

  delegateInputChan <- liftIO . atomically $ do
    q <- newTQueue
    writeTQueue q (DelegateEvent Start)
    pure q
  delegateBroadCastChan <- liftIO . atomically $ newTChan

  _ <-
    liftIO . forkIO $
      runServer (show $ dlgt'host conf) (fromIntegral $ dlgt'port conf) $
        runWithTracer' tracer . delegateServerApp (dlgt'ping conf) delegateInputChan delegateBroadCastChan

  execDelegateRunnerT $ mkRunner (dlgt'tick conf) delegateInputChan delegateBroadCastChan

main :: IO ()
main = do
  port <- lookupEnv "PORT"

  let conf :: DelegateServerConfig
      conf =
        DelegateServerConfig
          { dlgt'host = host
          , dlgt'port = fromMaybe 8080 $ port >>= readMaybe
          , dlgt'tick = tick
          , dlgt'ping = 30
          }
  runWithTracer' tracer $ runDelegateServer conf
  where
    tracer :: Tracer IO DelegateServerLog
    tracer = contramap (show . pretty) stdoutTracer

    host :: IP
    host = "127.0.0.1"

    tick :: Int
    tick = 1_000
