module HydraAuctionUtils.Hydra.Runner (
  HydraRunner (..),
  prepareScriptRegistry,
  HydraExecutionContext (..),
  executeHydraRunnerFakingParams,
  executeHydraRunner,
  runL1RunnerInComposite,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Tracer (Tracer, stdoutTracer, traceWith)
import Data.Aeson ()
import Data.ByteString.UTF8 as BSU
import Data.Text (Text)
import Network.HTTP.Req qualified as Req
import System.Environment (lookupEnv)
import System.Timeout (timeout)

-- Cardano imports

import Cardano.Api ()
import CardanoNode (RunningNode (..))

-- Hydra imports

import Hydra.API.HTTPServer (DraftCommitTxResponse (..))
import Hydra.Cardano.Api (
  AsType (AsTxId),
  FromJSON,
  -- ShelleyBasedEra (ShelleyBasedEraShelley),
  -- ToJSON,
  TxId,
  deserialiseFromRawBytesHex,
  -- fromLedgerPParams,
  toLedgerPParams,
  -- pattern LedgerProtocolParameters,
  pattern ShelleyBasedEraBabbage,
 )
import Hydra.Chain.Direct.ScriptRegistry (
  ScriptRegistry (..),
  publishHydraScripts,
  queryScriptRegistry,
 )
-- import Hydra.Ledger.Cardano.Evaluate (pparams)
import Hydra.Network (Host (..))

-- HydraAuction imports

import HydraAuctionUtils.BundledData (readHydraNodeProtocolParams)
import HydraAuctionUtils.Fixture (Actor (..), keysFor)
import HydraAuctionUtils.Hydra.Interface (
  HydraCommand,
  HydraEvent,
  HydraProtocol,
 )
import HydraAuctionUtils.Hydra.Monad (
  AwaitedHydraEvent,
  MonadHydra (..),
  ViaMonadHydra (..),
 )
import HydraAuctionUtils.L1.Runner (
  ExecutionContext (..),
  L1Runner,
  executeL1Runner,
 )
import HydraAuctionUtils.Monads (
  BlockchainParams (..),
  MonadBlockchainParams (..),
  MonadNetworkId (askNetworkId),
  MonadQueryUtxo (..),
  MonadSubmitTx (..),
  MonadTrace (..),
 )
import HydraAuctionUtils.Monads.Actors (
  MonadHasActor (..),
  WithActorT,
  withActor,
 )
import HydraAuctionUtils.Types.Natural (Natural, naturalToInt)
import HydraAuctionUtils.WebSockets.Client (
  AwaitedOutput,
  RealProtocolClient (..),
  waitForMatchingOutputH,
 )
import HydraAuctionUtils.WebSockets.Protocol (ProtocolClient (..))

data HydraRunnerLog
  = HydraRunnerStringMessage String
  | SendCommand HydraCommand
  | AwaitingHydraEvent AwaitedHydraEvent
  | AwaitedHydraEvent HydraEvent
  deriving stock (Show)

data HydraExecutionContext = MkHydraExecutionContext
  { node :: RealProtocolClient HydraProtocol
  , tracer :: Tracer IO HydraRunnerLog
  , l1Context :: ExecutionContext
  , actor :: Actor
  , scriptRegistry :: ScriptRegistry
  }

newtype HydraRunner a = MkHydraRunner
  {unHydraRunner :: ReaderT HydraExecutionContext IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader HydraExecutionContext
    , MonadBase IO
    , MonadBaseControl IO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )
  deriving (MonadSubmitTx, MonadQueryUtxo) via (ViaMonadHydra HydraRunner)

hydraHttpReq ::
  forall req resp method.
  ( Req.HttpBody req
  , Req.HttpMethod method
  , FromJSON resp
  , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody req)
  ) =>
  method ->
  Text ->
  req ->
  HydraRunner resp
hydraHttpReq method path reqBody = do
  MkHydraExecutionContext {node} <- ask
  let
    hostname' = hostname $ host node
    port' = port $ host node
    httpRequest =
      Req.req
        method
        (Req.http hostname' Req./: path)
        reqBody
        Req.jsonResponse
        (Req.port $ fromIntegral port')
  Req.runReq Req.defaultHttpConfig $
    Req.responseBody <$> httpRequest

instance MonadHydra HydraRunner where
  sendCommand :: HydraCommand -> HydraRunner ()
  sendCommand command = do
    MkHydraExecutionContext {node} <- ask
    traceMessage $ SendCommand command
    liftIO $ sendInputH node command

  createCommitTx request = do
    DraftCommitTxResponse {commitTx} <-
      hydraHttpReq Req.POST "commit" $ Req.ReqBodyJson request
    return commitTx

  waitForHydraEvent' ::
    Natural -> AwaitedOutput HydraProtocol -> HydraRunner (Maybe HydraEvent)
  waitForHydraEvent' timeoutSeconds awaitedSpec = do
    MkHydraExecutionContext {node} <- ask
    traceMessage $ AwaitingHydraEvent awaitedSpec
    mResult <-
      liftIO $
        timeout timeoutMicroseconds $
          waitForMatchingOutputH node awaitedSpec
    case mResult of
      Just result -> traceMessage $ AwaitedHydraEvent result
      Nothing -> return ()
    return mResult
    where
      timeoutMicroseconds =
        1_000_000 * fromInteger (toInteger $ naturalToInt timeoutSeconds)

  runL1RunnerInComposite action = do
    actor <- askActor
    MkHydraExecutionContext {l1Context} <- ask
    liftIO $ executeL1Runner l1Context $ withActor actor action

instance MonadNetworkId HydraRunner where
  askNetworkId = runL1RunnerInComposite askNetworkId

instance MonadTrace HydraRunner where
  type TracerMessage HydraRunner = HydraRunnerLog
  stringToMessage = HydraRunnerStringMessage
  traceMessage message = do
    MkHydraExecutionContext {tracer} <- ask
    liftIO $ traceWith tracer message

instance MonadBlockchainParams HydraRunner where
  -- FIXME: caching
  queryBlockchainParams = do
    params <- runL1RunnerInComposite queryBlockchainParams

    --  <- hydraHttpReq Req.GET "protocol-parameters" Req.NoReqBody
    -- putStrLn $ (bs :: String)

    pp' <- liftIO readHydraNodeProtocolParams
    let protocolParameters = either (error "TODO") id $ toLedgerPParams ShelleyBasedEraBabbage pp'
    return $ params {protocolParameters}

  queryCurrentSlot = runL1RunnerInComposite queryCurrentSlot
  convertValidityBound = runL1RunnerInComposite . convertValidityBound

  {-
  \| This records stats to underlying L1 Runner
  Not sure if this works well, but seems to match current L2 interation test
  -}
  recordTxStat = runL1RunnerInComposite . recordTxStat

instance MonadHasActor HydraRunner where
  askActor = actor <$> ask

executeHydraRunner ::
  HydraExecutionContext ->
  HydraRunner a ->
  IO a
executeHydraRunner context runner =
  runReaderT (unHydraRunner runner) context

-- | Loads known Hydra ScriptRegistry or deploys new
prepareScriptRegistry ::
  HasCallStack => Maybe TxId -> L1Runner (TxId, ScriptRegistry)
prepareScriptRegistry mScriptsTxId = do
  MkExecutionContext {node} <- ask
  let RunningNode {networkId, nodeSocket} = node
  !hydraScriptsTxId <- case mScriptsTxId of
    Just txId -> return txId
    Nothing -> liftIO $ do
      (_, sk) <- keysFor Faucet
      publishHydraScripts networkId nodeSocket sk
  scriptRegistry <-
    liftIO $
      queryScriptRegistry networkId nodeSocket hydraScriptsTxId
  pure (hydraScriptsTxId, scriptRegistry)

-- | Loads Hydra ScriptRegistry from HYDRA_SCRIPTS_TX_ID env, or deploys new
executeHydraRunnerFakingParams ::
  forall a.
  RealProtocolClient HydraProtocol ->
  HydraRunner a ->
  WithActorT L1Runner a
executeHydraRunnerFakingParams node monad = do
  mTxIdStr <- liftIO $ lookupEnv "HYDRA_SCRIPTS_TX_ID"
  let mTxId = deserializeTxId =<< mTxIdStr
  (_, scriptRegistry) <- lift $ prepareScriptRegistry mTxId
  l1Context <- lift ask
  actor <- askActor
  let context =
        MkHydraExecutionContext {node, tracer, l1Context, actor, scriptRegistry}
  liftIO $ executeHydraRunner context monad
  where
    tracer = contramap show stdoutTracer
    deserializeTxId =
      hush . deserialiseFromRawBytesHex AsTxId . BSU.fromString
