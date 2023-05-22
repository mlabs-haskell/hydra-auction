module HydraAuctionUtils.L1.Runner (
  HydraAuctionLog (..),
  EndToEndLog (..),
  NodeLog (..),
  L1Runner,
  executeL1Runner,
  executeL1RunnerWithNodeAs,
  executeTestL1Runner,
  dockerNode,
  StateDirectory (..),
  ExecutionContext (..),
  withActor,
  fileTracer,
  toSlotNo,
  initWallet,
  stdoutOrNullTracer,
) where

-- Prelude imports

import Hydra.Prelude (
  MonadIO,
  MonadReader,
  ReaderT,
  ask,
  contramap,
  liftIO,
  local,
  runReaderT,
 )
import Test.Hydra.Prelude (withTempDir)
import Prelude

-- Haskell imports

import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Tracer (nullTracer, stdoutTracer, traceWith)

-- Cardano imports
import CardanoClient (
  QueryPoint (QueryTip),
  awaitTransaction,
  queryEraHistory,
  queryProtocolParameters,
  queryStakePools,
  querySystemStart,
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
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Chain.Direct.TimeHandle (TimeHandle (..), queryTimeHandle)
import Plutus.V2.Ledger.Api (POSIXTime (getPOSIXTime))

-- Hydra imports
import Hydra.Cardano.Api (
  Lovelace,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  SlotNo,
  Tx,
  UTxO,
  pattern TxValidityLowerBound,
  pattern TxValidityNoLowerBound,
  pattern TxValidityNoUpperBound,
  pattern TxValidityUpperBound,
 )
import Hydra.Cluster.Faucet (Marked (Normal), seedFromFaucet)
import Hydra.Logging (Tracer)
import HydraNode (EndToEndLog (FromCardanoNode, FromFaucet))

-- Hydra auction imports

import HydraAuctionUtils.Fixture (Actor (..), keysFor)
import HydraAuctionUtils.L1.Runner.Tracer (
  HydraAuctionLog (..),
  StateDirectory (..),
  fileTracer,
  stdoutOrNullTracer,
 )
import HydraAuctionUtils.Monads (
  BlockchainParams (..),
  MonadBlockchainParams (..),
  MonadNetworkId (..),
  MonadQueryUtxo (..),
  MonadSubmitTx (..),
  MonadTrace (..),
  UtxoQuery (..),
 )
import HydraAuctionUtils.Monads.Actors (MonadHasActor (..))

{- | Execution context holding the current tracer,
 as well as the running node.
-}
data ExecutionContext = MkExecutionContext
  { tracer :: !(Tracer IO HydraAuctionLog)
  , node :: !RunningNode
  , actor :: !Actor
  }

{- | HydraAuction specific L1 computation executor.
     Knows about L1 connection and current actor.
-}
newtype L1Runner a = MkRunner
  {run :: ReaderT ExecutionContext IO a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader ExecutionContext
    , MonadThrow
    , MonadCatch
    , MonadMask
    )
    via ReaderT ExecutionContext IO

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

  convertValidityBound (lowerBound', upperBound') = do
    lowerBound <- case lowerBound' of
      Nothing -> pure TxValidityNoLowerBound
      Just x -> TxValidityLowerBound <$> toSlotNo x
    upperBound <- case upperBound' of
      Nothing -> pure TxValidityNoUpperBound
      Just x -> TxValidityUpperBound <$> toSlotNo x
    return (lowerBound, upperBound)

toSlotNo :: POSIXTime -> L1Runner SlotNo
toSlotNo ptime = do
  MkExecutionContext {node} <- ask
  let RunningNode {networkId, nodeSocket} = node
  timeHandle <-
    liftIO $ queryTimeHandle networkId nodeSocket
  let timeInSeconds = getPOSIXTime ptime `div` 1000
      ndtime = secondsToNominalDiffTime $ fromInteger timeInSeconds
      utcTime = posixSecondsToUTCTime ndtime
  either (error . show) return $
    slotFromUTCTime timeHandle utcTime

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

instance MonadHasActor L1Runner where
  askActor = do
    MkExecutionContext {actor} <- ask
    return actor

executeL1Runner ::
  ExecutionContext ->
  L1Runner a ->
  IO a
executeL1Runner context runner =
  runReaderT (run runner) context

withActor :: Actor -> L1Runner a -> L1Runner a
withActor actor = local (\ctx -> ctx {actor = actor})

-- | Executes a test runner using a temporary directory as the @StateDirectory@.
executeTestL1Runner :: L1Runner () -> IO ()
executeTestL1Runner runner = do
  withTempDir "test-hydra-auction" $ \tmpDir -> do
    withCardanoNodeDevnet
      nullTracer
      tmpDir
      $ \node ->
        executeL1RunnerWithNodeAs node Alice runner

dockerNode :: RunningNode
dockerNode =
  RunningNode
    { networkId = Testnet $ NetworkMagic 42
    , nodeSocket = "./devnet/node.socket"
    }

executeL1RunnerWithNodeAs :: forall x. RunningNode -> Actor -> L1Runner x -> IO x
executeL1RunnerWithNodeAs node actor runner = do
  let tracer = contramap show stdoutTracer
  executeL1Runner
    (MkExecutionContext {tracer = tracer, node, actor})
    runner

-- * Utils

{- | Initiates the actor's wallet using the prescribed amount of faucet
 @Lovelace@.
-}
initWallet :: Lovelace -> Actor -> L1Runner UTxO
initWallet amount actor = do
  MkExecutionContext {node} <- ask
  liftIO $ do
    (vk, _) <- keysFor actor
    seedFromFaucet
      node
      vk
      amount
      Normal
      nullTracer
