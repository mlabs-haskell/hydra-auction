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
  logMsg,
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
  String,
  ask,
  contramap,
  liftIO,
  local,
  runReaderT,
  ($),
  (.),
 )
import Test.Hydra.Prelude (withTempDir)

-- Haskell imports
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Tracer (traceWith)

-- Hydra imports
import CardanoNode (
  NodeLog (..),
  RunningNode,
  withCardanoNodeDevnet,
 )
import Hydra.Cardano.Api (Lovelace)
import Hydra.Cluster.Faucet (Marked (Normal), seedFromFaucet_)
import Hydra.Logging (Tracer)
import HydraNode (EndToEndLog (FromCardanoNode, FromFaucet))

-- Hydra auction imports
import HydraAuction.Fixture (Actor (..), keysFor)
import HydraAuction.Runner.Tracer (
  HydraAuctionLog (..),
  StateDirectory (..),
  fileTracer,
  showLogsOnFailure,
  stdoutOrNullTracer,
 )

{- | Execution context holding the current tracer,
 as well as the running node.
-}
data ExecutionContext = MkExecutionContext
  { tracer :: !(Tracer IO HydraAuctionLog)
  , node :: !RunningNode
  , actor :: !Actor
  }

{- | Hydra computation executor. Note that @Runner@ is
 de facto `ReaderT ExecutionContext IO`.
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

executeRunner ::
  ExecutionContext ->
  Runner a ->
  IO a
executeRunner context runner =
  runReaderT (run runner) context

withActor :: Actor -> Runner a -> Runner a
withActor actor = local (\ctx -> ctx {actor = actor})

logMsg :: String -> Runner ()
logMsg s = do
  MkExecutionContext {tracer} <- ask
  liftIO $ traceWith tracer (FromHydraAuction s)

-- | Executes a test runner using a temporary directory as the @StateDirectory@.
executeTestRunner :: Runner () -> IO ()
executeTestRunner runner = do
  withTempDir "test-hydra-auction" $ \tmpDir -> do
    let stateDirectory = MkStateDirectory tmpDir
    tracerForCardanoNode <- fileTracer stateDirectory
    withCardanoNodeDevnet
      (contramap (FromHydra . FromCardanoNode) tracerForCardanoNode)
      tmpDir
      $ \node -> showLogsOnFailure $ \tracer ->
        executeRunner
          (MkExecutionContext {tracer = tracer, node = node, actor = Alice})
          runner

-- * Utils

{- | Initiates the actor's wallet using the prescribed amount of faucet
 @Lovelace@.
-}
initWallet :: Lovelace -> Actor -> Runner ()
initWallet amount actor = do
  MkExecutionContext {tracer, node} <- ask
  liftIO $ do
    (vk, _) <- keysFor actor
    seedFromFaucet_
      node
      vk
      amount
      Normal
      (contramap (FromHydra . FromFaucet) tracer)
