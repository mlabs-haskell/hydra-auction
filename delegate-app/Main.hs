module Main (main) where

-- Prelude imports
import Hydra.Cardano.Api.Prelude (MonadReader (ask), lookupEnv)
import Hydra.Prelude (race_, readMaybe, traverse_)
import Prelude

-- Haskell imports
import Control.Concurrent (threadDelay)
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
import Control.Tracer (Tracer, contramap, stdoutTracer, traceWith)
import Data.Aeson (decode, encode)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (
  acceptRequest,
  defaultConnectionOptions,
  receiveData,
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
  DelegateServerConfig (DelegateServerConfig, dlgt'host, dlgt'port, dlgt'tick),
  DelegateServerLog (DelegateError, DelegateOutput, FrontendConnected, FrontendInput, Started),
  MonadTracer (trace),
  ServerAppT,
  TracerT,
  askTracer,
  dlgt'ping,
  dlgt'tick,
  runWithTracer',
 )

delegateServerApp ::
  ( MonadIO m
  , MonadTracer DelegateServerLog m
  , MonadReader (Tracer IO DelegateServerLog) m
  ) =>
  Int ->
  TQueue DelegateInput ->
  TChan DelegateResponse ->
  ServerAppT m
delegateServerApp pingSecs reqq broadcast pending = do
  connection <- liftIO $ acceptRequest pending
  trace FrontendConnected

  tracer <- ask
  liftIO $ do
    broadcast' <- atomically $ dupTChan broadcast

    let receive = forever $ do
          inp <- receiveData connection
          case decode @FrontendRequest inp of
            Nothing -> traceWith tracer $ DelegateError FrontendNoParse
            Just req -> do
              traceWith tracer $ FrontendInput req
              atomically $ writeTQueue reqq $ FrontendRequest req
        send =
          forever $
            atomically (readTChan broadcast')
              >>= sendTextData connection . encode

    withPingThread connection pingSecs (pure ()) $ race_ receive send -- TODO: can we do this

-- liftIO (receiveData connection)
--   >>= ( \case
--           Nothing -> trace (DelegateError FrontendNoParse)
--           Just req -> do
--             trace $ FrontendInput req
--             -- delegateStep will enqueue a request
--       )
--     . decode
-- FIXME: remove concrete monads and make this tagless final (needs MonadDelegate)
mkRunner ::
  forall m.
  (MonadIO m, MonadTracer DelegateServerLog m) =>
  Int ->
  TQueue DelegateInput ->
  TChan DelegateResponse ->
  DelegateRunnerT m ()
-- FIXME: we need to abort at some point but this doesn't seem
-- to be implemented yet so we just go on
mkRunner tick reqq broadcast = forever $ do
  -- FIXME: more elaborate logic for eventsProducer
  let encodeBroadcast :: [DelegateResponse] -> DelegateRunnerT m ()
      encodeBroadcast responses = do
        liftIO . atomically $ traverse_ (writeTChan broadcast) responses
        lift $ traverse_ (trace . DelegateOutput) responses

  events <- liftIO $ do
    atomically $ flushTQueue reqq

  traverse_ (delegateStep >=> encodeBroadcast) events

  liftIO $ threadDelay tick

runDelegateServer ::
  DelegateServerConfig ->
  TracerT DelegateServerLog IO ()
runDelegateServer conf = do
  trace (Started $ dlgt'port conf)

  let fallback :: Application
      fallback _req res =
        res $
          responseLBS status200 [("Content-Type", "text/plain")] "Websocket endpoint of delegate server"

  tracer <- askTracer

  delegateInputChan <- liftIO . atomically $ do
    q <- newTQueue
    writeTQueue q (DelegateEvent Start)
    pure q
  delegateBroadCastChan <- liftIO . atomically $ newTChan

  execDelegateRunnerT $ mkRunner (dlgt'tick conf) delegateInputChan delegateBroadCastChan

  liftIO $
    run (fromIntegral $ dlgt'port conf) $
      flip (websocketsOr defaultConnectionOptions) fallback $
        runWithTracer' tracer . delegateServerApp (dlgt'ping conf) delegateInputChan delegateBroadCastChan

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
