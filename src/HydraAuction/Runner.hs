module HydraAuction.Runner (
  HydraAuctionLog (..),
  EndToEndLog (..),
  NodeLog (..),
  Runner,
  executeRunner,
  executeRunnerWithNodeAs,
  executeTestRunner,
  executeDockerRunner,
  dockerNode,
  StateDirectory (..),
  ExecutionContext (..),
  withActor,
  fileTracer,
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
import System.Process.Typed (runProcess_)

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
  Tx,
  UTxO,
 )
import Hydra.Cluster.Faucet (Marked (Normal), seedFromFaucet)
import Hydra.Logging (Tracer)
import HydraNode (EndToEndLog (FromCardanoNode, FromFaucet))

-- Hydra auction imports
import HydraAuction.Runner.Tracer (
  HydraAuctionLog (..),
  StateDirectory (..),
  fileTracer,
  stdoutOrNullTracer,
 )
import HydraAuctionUtils.Fixture (Actor (..), keysFor)
import HydraAuctionUtils.Monads (
  BlockchainParams (..),
  MonadBlockchainParams (..),
  MonadHasActor (..),
  MonadNetworkId (..),
  MonadQueryUtxo (..),
  MonadSubmitTx (..),
  MonadTrace (..),
  UtxoQuery (..),
 )

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
newtype Runner a = MkRunner
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

instance MonadQueryUtxo Runner where
  queryUtxo query = do
    MkExecutionContext {node} <- ask
    let RunningNode {networkId, nodeSocket} = node
    liftIO $ case query of
      ByTxIns txIns ->
        queryUTxOByTxIn networkId nodeSocket QueryTip txIns
      ByAddress address ->
        queryUTxO networkId nodeSocket QueryTip [address]

instance MonadNetworkId Runner where
  askNetworkId = do
    MkExecutionContext {node} <- ask
    let RunningNode {networkId} = node
    return networkId

instance MonadBlockchainParams Runner where
  queryBlockchainParams :: Runner BlockchainParams
  queryBlockchainParams = do
    MkExecutionContext {node} <- ask
    let RunningNode {networkId, nodeSocket} = node
    liftIO $
      MkBlockchainParams
        <$> queryProtocolParameters networkId nodeSocket QueryTip
        <*> querySystemStart networkId nodeSocket QueryTip
        <*> queryEraHistory networkId nodeSocket QueryTip
        <*> queryStakePools networkId nodeSocket QueryTip

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

instance MonadSubmitTx Runner where
  submitTx = callWithTx submitTransaction
  awaitTx = callWithTx (\nId nS tx -> void $ awaitTransaction nId nS tx)

instance MonadTrace Runner where
  type TracerMessage Runner = HydraAuctionLog
  stringToMessage = FromHydraAuction
  traceMessage message = do
    MkExecutionContext {tracer} <- ask
    liftIO $ traceWith tracer message

instance MonadHasActor Runner where
  askActor = do
    MkExecutionContext {actor} <- ask
    return actor

executeRunner ::
  ExecutionContext ->
  Runner a ->
  IO a
executeRunner context runner =
  runReaderT (run runner) context

withActor :: Actor -> Runner a -> Runner a
withActor actor = local (\ctx -> ctx {actor = actor})

-- | Executes a test runner using a temporary directory as the @StateDirectory@.
executeTestRunner :: Runner () -> IO ()
executeTestRunner runner = do
  withTempDir "test-hydra-auction" $ \tmpDir -> do
    withCardanoNodeDevnet
      nullTracer
      tmpDir
      $ \node ->
        executeRunnerWithNodeAs node Alice runner

dockerNode :: RunningNode
dockerNode =
  RunningNode
    { networkId = Testnet $ NetworkMagic 42
    , nodeSocket = "./devnet/node.socket"
    }

executeDockerRunner :: Runner () -> IO ()
executeDockerRunner runner = do
  runProcess_ "make start-docker"
  executeRunnerWithNodeAs dockerNode Alice runner

executeRunnerWithNodeAs :: forall x. RunningNode -> Actor -> Runner x -> IO x
executeRunnerWithNodeAs node actor runner = do
  let tracer = contramap show stdoutTracer
  executeRunner
    (MkExecutionContext {tracer = tracer, node, actor})
    runner

-- * Utils

{- | Initiates the actor's wallet using the prescribed amount of faucet
 @Lovelace@.
-}
initWallet :: Lovelace -> Actor -> Runner UTxO
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
