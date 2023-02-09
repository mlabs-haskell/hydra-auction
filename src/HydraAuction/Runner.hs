{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Runner (
  Runner,
  executeRunner,
  executeTestRunner,
  StateDirectory (..),
  ExecutionContext (..),
  fileTracer,
  initWallet,
  stdoutTracer,
  logMsg,
) where

import CardanoNode (
  RunningNode,
  withCardanoNodeDevnet,
 )

import Hydra.Cardano.Api (Lovelace)
import Hydra.Cluster.Faucet (Marked (Normal), seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor)
import Hydra.Cluster.Util (keysFor)
import Hydra.Logging (
  Tracer,
  Verbosity,
  withTracer,
  withTracerOutputTo,
 )
import Hydra.Prelude (
  Applicative (pure),
  Bool (False),
  Contravariant (contramap),
  FilePath,
  Functor,
  IO,
  IOMode (ReadWriteMode),
  Monad,
  MonadFail (..),
  MonadIO (..),
  MonadReader (ask),
  ReaderT (..),
  String,
  when,
  withFile,
  ($),
 )

import HydraNode (EndToEndLog (FromCardanoNode, FromFaucet))

import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Test.Hydra.Prelude (withTempDir)

{- | Execution context holding the current tracer,
 as well as the running node.
-}
data ExecutionContext = MkExecutionContext
  { tracer :: !(Tracer IO EndToEndLog)
  , node :: !RunningNode
  , verbose :: !Bool
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
  Tracer IO EndToEndLog ->
  RunningNode ->
  Bool ->
  Runner a ->
  IO a
executeRunner tracer node verbose runner =
  runReaderT (run runner) (MkExecutionContext tracer node verbose)

{- | Filter tracer which logs into a `test.log` file within the given
 @StateDirectory@.
-}
fileTracer :: StateDirectory -> IO (Tracer IO EndToEndLog)
fileTracer MkStateDirectory {..} = do
  withFile (stateDirectory </> "test.log") ReadWriteMode $ \h ->
    withTracerOutputTo h "Tracer" $ \tracer -> pure tracer

-- | Stdout tracer using the given verbosity level.
stdoutTracer :: Verbosity -> IO (Tracer IO EndToEndLog)
stdoutTracer verbosity =
  withTracer verbosity $ \tracer -> pure tracer

logMsg :: String -> Runner ()
logMsg s = do
  MkExecutionContext {..} <- ask
  when verbose $
    liftIO $ hPutStrLn stderr s

-- | Executes a test runner using a temporary directory as the @StateDirectory@.
executeTestRunner :: Runner () -> IO ()
executeTestRunner runner = do
  withTempDir "test-hydra-auction" $ \tmpDir -> do
    let stateDirectory = MkStateDirectory tmpDir
    tracer <- fileTracer stateDirectory
    withCardanoNodeDevnet
      (contramap FromCardanoNode tracer)
      tmpDir
      $ \node -> executeRunner tracer node False runner

-- | @FilePath@ used to store the running node data.
newtype StateDirectory = MkStateDirectory
  {stateDirectory :: FilePath}

-- * Utils

{- | Initiates the actor's wallet using the prescribed amount of faucet
 @Lovelace@.
-}
initWallet :: Actor -> Lovelace -> Runner ()
initWallet actor amount = do
  MkExecutionContext {..} <- ask
  liftIO $ do
    (vk, _) <- keysFor actor
    seedFromFaucet_
      node
      vk
      amount
      Normal
      (contramap FromFaucet tracer)
