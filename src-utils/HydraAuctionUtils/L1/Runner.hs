module HydraAuctionUtils.L1.Runner (
  feeSpent,
  queryAdaWithoutFees,
  HydraAuctionLog (..),
  EndToEndLog (..),
  NodeLog (..),
  L1Runner,
  executeL1Runner,
  executeL1RunnerWithNodeAs,
  executeL1RunnerWithNode,
  executeTestL1Runner,
  dockerNode,
  StateDirectory (..),
  ExecutionContext (..),
  -- FIXME: reexport
  withActor,
  fileTracer,
  toSlotNo,
  initWallet,
  stdoutOrNullTracer,
) where

-- Prelude imports

import HydraAuctionUtils.Prelude
import Test.Hydra.Prelude (withTempDir)

-- Haskell imports

import Control.Tracer (nullTracer, stdoutTracer, traceWith)
import Data.List (isInfixOf)
import System.Timeout (timeout)

-- Cardano imports
import CardanoClient (
  QueryPoint (..),
  awaitTransaction,
  localNodeConnectInfo,
  queryEraHistory,
  queryProtocolParameters,
  queryStakePools,
  querySystemStart,
  queryTip,
  queryUTxO,
  queryUTxOByTxIn,
 )
import CardanoNode (
  NodeLog (..),
  RunningNode (RunningNode, networkId, nodeSocket),
  withCardanoNodeDevnet,
 )

-- Plutus imports
import PlutusLedgerApi.V2 (Extended (..), Interval (..), LowerBound (..), POSIXTime (..), UpperBound (..))

-- Hydra imports

import Hydra.Cardano.Api (
  ChainPoint (..),
  Lovelace,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  SlotNo (..),
  SubmitResult (..),
  TxValidityLowerBound,
  TxValidityUpperBound,
  getTxBody,
  submitTxToNodeLocal,
  txValidityRange,
  pattern BabbageEraInCardanoMode,
  pattern TxBody,
  pattern TxInMode,
  pattern TxValidityLowerBound,
  pattern TxValidityNoLowerBound,
  pattern TxValidityNoUpperBound,
  pattern TxValidityUpperBound,
 )
import Hydra.Cluster.Faucet (Marked (Normal))
import Hydra.Logging (Tracer)
import HydraNode (EndToEndLog (FromCardanoNode, FromFaucet))

-- Hydra auction imports

import HydraAuctionUtils.Fixture (Actor (..))
import HydraAuctionUtils.L1.Runner.Tracer (
  HydraAuctionLog (..),
  StateDirectory (..),
  fileTracer,
  stdoutOrNullTracer,
 )
import HydraAuctionUtils.Monads (
  BlockchainParams (..),
  MonadBlockchainParams (..),
  MonadCardanoClient,
  MonadNetworkId (..),
  MonadQueryUtxo (..),
  MonadSubmitTx (..),
  MonadTrace (..),
  SubmitingError (..),
  TxStat (..),
  UtxoQuery (..),
  askL1Timeout,
  toSlotNo,
  waitUntilSlot,
 )
import HydraAuctionUtils.Monads.Actors (
  MonadHasActor (..),
  WithActorT,
  actorTipUtxo,
  withActor,
 )
import HydraAuctionUtils.Tx.Common (transferAda, utxoLovelaceValue)

{- | Execution context holding the current tracer,
 as well as the running node.
-}
data ExecutionContext = MkExecutionContext
  { tracer :: Tracer IO HydraAuctionLog
  , node :: RunningNode
  , recordedStatsVar :: MVar [TxStat]
  }

-- | HydraAuction specific L1 computation executor.
newtype L1Runner a = MkRunner
  {run :: ReaderT ExecutionContext IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader ExecutionContext
    , MonadBase IO
    , MonadBaseControl IO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance MonadQueryUtxo L1Runner where
  queryUtxo query =
    case query of
      ByTxIns txIns ->
        runQuery queryUTxOByTxIn txIns
      ByAddress address ->
        runQuery queryUTxO [address]
    where
      runQuery f v = runWithNetworkParams (applyValue f v) QueryTip
      applyValue f v x y z = f x y z v

instance MonadNetworkId L1Runner where
  askNetworkId = networkId . node <$> ask

instance MonadBlockchainParams L1Runner where
  queryBlockchainParams :: L1Runner BlockchainParams
  queryBlockchainParams = do
    MkBlockchainParams
      <$> runQuery queryProtocolParameters
      <*> runQuery querySystemStart
      <*> runQuery queryEraHistory
      <*> runQuery queryStakePools
    where
      runQuery f = runWithNetworkParams f QueryTip

  convertValidityBound (Interval l u) = (,) <$> lowerBoundToValidityBound l <*> upperBoundToValidityBound u
    where
      closureToInteger :: Bool -> Integer
      closureToInteger = toInteger . fromEnum . not

      lowerBoundToValidityBound :: LowerBound POSIXTime -> L1Runner TxValidityLowerBound
      lowerBoundToValidityBound (LowerBound NegInf _) = pure TxValidityNoLowerBound
      lowerBoundToValidityBound (LowerBound (Finite n) c) = TxValidityLowerBound <$> toSlotNo (POSIXTime $ getPOSIXTime n + closureToInteger c)
      lowerBoundToValidityBound (LowerBound PosInf _) = error "Unable to create posinf lower bound"

      upperBoundToValidityBound :: UpperBound POSIXTime -> L1Runner TxValidityUpperBound
      upperBoundToValidityBound (UpperBound NegInf _) = error "Unable to create neginf upper bound"
      upperBoundToValidityBound (UpperBound (Finite n) c) = TxValidityUpperBound <$> toSlotNo (POSIXTime $ getPOSIXTime n - closureToInteger c)
      upperBoundToValidityBound (UpperBound PosInf _) = pure TxValidityNoUpperBound

  queryCurrentSlot :: L1Runner SlotNo
  queryCurrentSlot = do
    MkExecutionContext {node} <- ask
    tip <- liftIO $ queryTip (networkId node) (nodeSocket node)
    return $
      case tip of
        ChainPointAtGenesis -> SlotNo 0
        ChainPoint slotNo _ -> slotNo

  recordTxStat stat = do
    MkExecutionContext {recordedStatsVar} <- ask
    liftIO $ modifyMVar_ recordedStatsVar (return . (stat :))

runWithNetworkParams ::
  (MonadReader ExecutionContext m, MonadIO m) =>
  (NetworkId -> FilePath -> d -> IO b) ->
  d ->
  m b
runWithNetworkParams call arg = do
  MkExecutionContext {node} <- ask
  let RunningNode {networkId, nodeSocket} = node
  liftIO $
    call
      networkId
      nodeSocket
      arg

instance MonadSubmitTx L1Runner where
  submitTx tx = do
    waitForTxSlot
    timeoutSecs <- askL1Timeout
    context <- ask
    mResult <-
      liftIO $
        timeout (timeoutSecs * 1_000_000) $
          executeL1Runner context $
            runWithNetworkParams submitTx' ()
    return $ case mResult of
      Just result -> result
      Nothing -> Left Timeout
    where
      submitTx' networkId socket () = do
        result <-
          submitTxToNodeLocal
            (localNodeConnectInfo networkId socket)
            (TxInMode tx BabbageEraInCardanoMode)
        return $ case result of
          -- FIXME: encoding for errorData is too hard for me to understand
          SubmitFail errorData ->
            if "BadInputsUTxO" `isInfixOf` show errorData
              then Left InvalidatedTxIn
              else
                if "OutsideValidityIntervalUTxO" `isInfixOf` show errorData
                  then -- FIXME: actually it could be too early, but we check that
                    Left TooLate
                  else Left $ NotExpected $ show errorData
          SubmitSuccess -> Right ()
      TxBody bodyContent = getTxBody tx
      waitForTxSlot = do
        case fst (txValidityRange bodyContent) of
          TxValidityLowerBound slot -> waitUntilSlot slot
          TxValidityNoLowerBound -> return ()

  awaitTx = runWithNetworkParams (\nId nS tx -> void $ awaitTransaction nId nS tx)

instance MonadTrace L1Runner where
  type TracerMessage L1Runner = HydraAuctionLog
  stringToMessage = FromHydraAuction
  traceMessage message = do
    MkExecutionContext {tracer} <- ask
    liftIO $ traceWith tracer message

executeL1Runner ::
  ExecutionContext ->
  L1Runner a ->
  IO a
executeL1Runner context runner =
  runReaderT (run runner) context

-- | Executes a test runner using a temporary directory as the @StateDirectory@.
executeTestL1Runner :: L1Runner () -> IO ()
executeTestL1Runner runner = do
  withTempDir "test-hydra-auction" $ \tmpDir -> do
    withCardanoNodeDevnet
      nullTracer
      tmpDir
      $ \node ->
        executeL1RunnerWithNode node runner

dockerNode :: RunningNode
dockerNode =
  RunningNode
    { networkId = Testnet $ NetworkMagic 42
    , nodeSocket = "./devnet/node.socket"
    }

executeL1RunnerWithNode :: forall x. RunningNode -> L1Runner x -> IO x
executeL1RunnerWithNode node runner = do
  recordedStatsVar <- newMVar []
  let
    tracer = contramap show stdoutTracer
    context = MkExecutionContext {tracer = tracer, node, recordedStatsVar}
  executeL1Runner context runner

executeL1RunnerWithNodeAs :: forall x. RunningNode -> Actor -> WithActorT L1Runner x -> IO x
executeL1RunnerWithNodeAs node actor runner =
  executeL1RunnerWithNode node (withActor actor runner)

-- * Utils

{- | Initiates the actor's wallet using the prescribed amount of faucet
 @Lovelace@.
-}

-- FIXME: remove
initWallet ::
  forall m.
  (MonadIO m, MonadFail m, MonadTrace m, MonadCardanoClient m) =>
  Lovelace ->
  Actor ->
  m ()
initWallet amount actor = do
  void $ withActor Faucet $ transferAda actor Normal amount

feeSpent :: WithActorT L1Runner Lovelace
feeSpent = do
  MkExecutionContext {recordedStatsVar} <- lift ask
  stats <- liftIO $ readMVar recordedStatsVar
  actor <- askActor
  let relevantStats = filter (\x -> actor `elem` signers x) stats
  return $ sum $ map fee relevantStats

queryAdaWithoutFees :: WithActorT L1Runner Lovelace
queryAdaWithoutFees = do
  spent <- feeSpent
  amount <- utxoLovelaceValue <$> actorTipUtxo
  return $ amount + spent
