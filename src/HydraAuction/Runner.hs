module HydraAuction.Runner (
  HydraAuctionLog (..),
  EndToEndLog (..),
  NodeLog (..),
  Runner,
  executeRunner,
  executeTestRunner,
  StateDirectory (..),
  ExecutionContext (..),
  fileTracer,
  initWallet,
  stdoutOrNullTracer,
  logMsg,
) where

-- Prelude imports
import Hydra.Prelude
import Test.Hydra.Prelude (withTempDir)

-- Haskell imports
import Control.Tracer (traceWith)

-- Hydra imports
import CardanoNode (
  NodeLog (..),
  RunningNode,
  withCardanoNodeDevnet,
 )
import Hydra.Cardano.Api (Lovelace)
import Hydra.Cluster.Faucet (Marked (Normal), seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor)
import Hydra.Cluster.Util (keysFor)
import Hydra.Logging (Tracer)
import HydraNode (EndToEndLog (FromCardanoNode, FromFaucet))

-- Hydra auction imports
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
    )
    via ReaderT ExecutionContext IO

executeRunner ::
  ExecutionContext ->
  Runner a ->
  IO a
executeRunner context runner =
  runReaderT (run runner) context

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
          (MkExecutionContext {tracer = tracer, node = node})
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
