module HydraAuctionUtils.L1.Runner (
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

-- Cardano imports
import CardanoClient (
  QueryPoint (..),
  awaitTransaction,
  queryEraHistory,
  queryProtocolParameters,
  queryStakePools,
  querySystemStart,
  queryTip,
  queryUTxO,
  queryUTxOByTxIn,
  submitTransaction,
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
  Tx,
  TxValidityLowerBound,
  TxValidityUpperBound,
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
  UtxoQuery (..),
  toSlotNo,
 )
import HydraAuctionUtils.Monads.Actors (WithActorT, withActor)
import HydraAuctionUtils.Tx.Common (transferAda)

{- | Execution context holding the current tracer,
 as well as the running node.
-}
data ExecutionContext = MkExecutionContext
  { tracer :: !(Tracer IO HydraAuctionLog)
  , node :: !RunningNode
  }

{- | HydraAuction specific L1 computation executor.
     Knows about L1 connection and current actor.
-}
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
  queryUtxo query = do
    MkExecutionContext {node} <- ask
    let RunningNode {networkId, nodeSocket} = node
    liftIO $ case query of
      ByTxIns txIns ->
        queryUTxOByTxIn networkId nodeSocket QueryTip txIns
      ByAddress address ->
        queryUTxO networkId nodeSocket QueryTip [address]

instance MonadNetworkId L1Runner where
  askNetworkId = do
    MkExecutionContext {node} <- ask
    let RunningNode {networkId} = node
    return networkId

instance MonadBlockchainParams L1Runner where
  queryBlockchainParams :: L1Runner BlockchainParams
  queryBlockchainParams = do
    MkExecutionContext {node} <- ask
    let RunningNode {networkId, nodeSocket} = node
    liftIO $
      MkBlockchainParams
        <$> queryProtocolParameters networkId nodeSocket QueryTip
        <*> querySystemStart networkId nodeSocket QueryTip
        <*> queryEraHistory networkId nodeSocket QueryTip
        <*> queryStakePools networkId nodeSocket QueryTip

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

callWithTx ::
  (MonadReader ExecutionContext m, MonadIO m) =>
  (NetworkId -> FilePath -> Tx -> IO b) ->
  Tx ->
  m b
callWithTx call tx = do
  MkExecutionContext {node} <- ask
  let RunningNode {networkId, nodeSocket} = node
  liftIO $
    call
      networkId
      nodeSocket
      tx

instance MonadSubmitTx L1Runner where
  submitTx = callWithTx submitTransaction
  awaitTx = callWithTx (\nId nS tx -> void $ awaitTransaction nId nS tx)

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
  let tracer = contramap show stdoutTracer
  executeL1Runner (MkExecutionContext {tracer = tracer, node}) runner

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
initWallet amount actor =
  void $ withActor Faucet $ transferAda actor Normal amount
