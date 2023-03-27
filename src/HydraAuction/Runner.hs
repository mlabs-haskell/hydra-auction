module HydraAuction.Runner (
  HydraAuctionLog (..),
  EndToEndLog (..),
  NodeLog (..),
  Runner,
  executeRunner,
  executeTestRunner,
  StateDirectory (..),
  ExecutionContext (..),
  withActor,
  fileTracer,
  initWallet,
  stdoutOrNullTracer,
) where

-- Prelude imports
import Hydra.Prelude (
  Applicative,
  Functor,
  IO,
  Monad,
  MonadFail,
  MonadIO,
  MonadReader,
  ReaderT,
  ask,
  contramap,
  liftIO,
  local,
  runReaderT,
  show,
  ($),
  (.),
 )
import Test.Hydra.Prelude (withTempDir)
import Prelude (return)

-- Haskell imports

import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Tracer (stdoutTracer, traceWith)
import System.FilePath (FilePath)

-- Cardano imports
import CardanoClient (
  QueryPoint (QueryTip),
  awaitTransaction,
  queryUTxO,
  queryUTxOByTxIn,
  queryUTxOFor,
  submitTransaction,
 )
import CardanoNode (
  NodeLog (..),
  RunningNode (RunningNode, networkId, nodeSocket),
  withCardanoNodeDevnet,
 )
-- Hydra imports
import Hydra.Cardano.Api (Lovelace, NetworkId, Tx, TxIn, UTxO)
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
    let stateDirectory = MkStateDirectory tmpDir
    tracerForCardanoNode <- fileTracer stateDirectory
    let tracer = contramap show stdoutTracer
    withCardanoNodeDevnet
      (contramap (FromHydra . FromCardanoNode) tracerForCardanoNode)
      tmpDir
      $ \node ->
        executeRunner
          (MkExecutionContext {tracer = tracer, node = node, actor = Alice})
          runner

-- * Utils

{- | Initiates the actor's wallet using the prescribed amount of faucet
 @Lovelace@.
-}
initWallet :: Lovelace -> Actor -> Runner UTxO
initWallet amount actor = do
  MkExecutionContext {tracer, node} <- ask
  liftIO $ do
    (vk, _) <- keysFor actor
    seedFromFaucet
      node
      vk
      amount
      Normal
      (contramap (FromHydra . FromFaucet) tracer)
