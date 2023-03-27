module Main (main) where

-- Prelude imports

-- Prelude imports
import Hydra.Cardano.Api.Prelude (lookupEnv)
import Hydra.Prelude (readMaybe, traverse_)
import Prelude

-- Haskell imports
import Control.Concurrent (threadDelay)
import Control.Monad (forever, (>=>))
import Control.Monad.Extra (whenJust)
import Control.Monad.State (MonadState (get), StateT, evalStateT, put)
import Control.Monad.Trans
import Control.Tracer (Tracer, contramap, stdoutTracer)
import Data.Aeson (decode, encode)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
import Prettyprinter (Pretty (pretty))

-- Hydra imports
import Hydra.Network

-- HydraAuction imports
import HydraAuction.Delegate (
  DelegateEvent (Start),
  DelegateInput (DelegateEvent, FrontendRequest),
  DelegateRunnerT,
  delegateStep,
  execDelegateRunnerT,
 )
import HydraAuction.Delegate.Server (
  DelegateError (FrontendNoParse),
  DelegateServerConfig (DelegateServerConfig, dlgt'host, dlgt'port, dlgt'tick),
  DelegateServerLog (DelegateError, DelegateOutput, FrontendConnected, FrontendInput, Started),
  MonadTracer (trace),
  ServerAppT,
  TracerT,
  askTracer,
  dlgt'tick,
  runWithTracer',
 )

eventsProducer :: Monad m => Bool -> DelegateRunnerT m (Maybe DelegateEvent)
eventsProducer thisIsFirstRun =
  if thisIsFirstRun
    then return $ Just Start
    else return Nothing

delegateServerApp ::
  (MonadIO m, MonadTracer DelegateServerLog m) =>
  Int ->
  ServerAppT m
delegateServerApp tick pending = do
  connection <- liftIO $ acceptRequest pending
  trace FrontendConnected
  -- FIXME: we will probably have to do this in parallel, but how can we enqueue in the same
  -- state? probably have to remodel this
  execDelegateRunnerT $ flip evalStateT True $ mkRunner tick connection

-- FIXME: remove concrete monads and make this tagless final (needs MonadDelegate)
mkRunner ::
  (MonadIO m, MonadTracer DelegateServerLog m) =>
  Int ->
  Connection ->
  StateT Bool (DelegateRunnerT m) ()
-- FIXME: we need to abort at some point but this doesn't seem
-- to be implemented yet so we just go on
mkRunner tick con = forever $ do
  isFirstRun <- get
  _ <- lift $ do
    let encodeSend = liftIO . sendTextData con . encode
    liftIO (receiveData con)
      >>= ( \case
              Nothing -> lift $ trace (DelegateError FrontendNoParse)
              Just req -> do
                lift . trace $ FrontendInput req
                step <- delegateStep (FrontendRequest req)
                lift $ traverse_ (trace . DelegateOutput) step
                encodeSend step
          )
        -- yes, this is not nice, but hlint wants it so badly
        . decode

    -- FIXME: more elaborate logic for eventsProducer
    mdelegateEvent <- eventsProducer isFirstRun
    whenJust (DelegateEvent <$> mdelegateEvent) $
      delegateStep >=> encodeSend

  liftIO $ threadDelay tick
  put False

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
  liftIO $
    run (fromIntegral $ dlgt'port conf) $
      flip (websocketsOr defaultConnectionOptions) fallback $
        runWithTracer' tracer . delegateServerApp (dlgt'tick conf)

main :: IO ()
main = do
  -- FIXME: cover either case. It probably should not be transformer.
  port <- lookupEnv "port"

  -- FIXME: do we need this?
  let host :: IP = "127.0.0.1"
      tick :: Int = 1_000

      conf :: DelegateServerConfig
      conf =
        DelegateServerConfig
          { dlgt'host = host
          , dlgt'port = fromMaybe 8080 $ port >>= readMaybe
          , dlgt'tick = tick
          }
      tracer :: Tracer IO DelegateServerLog
      tracer = contramap (show . pretty) stdoutTracer

  runWithTracer' tracer $ runDelegateServer conf
