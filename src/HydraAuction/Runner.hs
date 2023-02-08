{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Runner (
  Runner,
  executeRunner,
  executeTestRunner,
  StateDirectory (..),
  ExecutionContext (..),
  fileTracer,
  initWallet,
  devnet,
) where

import CardanoNode (
  RunningNode,
  withCardanoNodeDevnet,
 )

import Hydra.Cardano.Api (Lovelace)
import Hydra.Cluster.Faucet (Marked (Normal), seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor)
import Hydra.Cluster.Util (keysFor)
import Hydra.Logging (Tracer, withTracerOutputTo)
import Hydra.Prelude (
  Applicative (pure),
  Contravariant (contramap),
  FilePath,
  Functor,
  IO,
  IOMode (ReadWriteMode),
  Monad,
  MonadIO (..),
  MonadReader (ask),
  ReaderT (..),
  withFile,
  ($),
 )

import HydraNode (EndToEndLog (FromCardanoNode, FromFaucet))

import System.FilePath ((</>))

import Test.Hydra.Prelude (withTempDir)

{- | Execution context holding the current tracer,
 as well as the running node.
-}
data ExecutionContext = MkExecutionContext
  { tracer :: !(Tracer IO EndToEndLog)
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
    , MonadReader ExecutionContext
    )
    via ReaderT ExecutionContext IO

executeRunner :: Tracer IO EndToEndLog -> RunningNode -> Runner a -> IO a
executeRunner tracer node runner =
  runReaderT (run runner) (MkExecutionContext tracer node)

fileTracer :: StateDirectory -> IO (Tracer IO EndToEndLog)
fileTracer MkStateDirectory {..} = do
  withFile (stateDirectory </> "test.log") ReadWriteMode $ \h ->
    withTracerOutputTo h "Tracer" $ \tracer -> pure tracer

devnet ::
  StateDirectory ->
  Tracer IO EndToEndLog ->
  (RunningNode -> IO ()) ->
  IO ()
devnet MkStateDirectory {..} tracer =
  withCardanoNodeDevnet
    (contramap FromCardanoNode tracer)
    stateDirectory

-- | Executes a test runner using a temporary directory as the @StateDirectory@.
executeTestRunner :: Runner () -> IO ()
executeTestRunner runner = do
  withTempDir "test-hydra-auction" $ \tmpDir -> do
    let stateDirectory = MkStateDirectory tmpDir
    tracer <- fileTracer stateDirectory
    devnet stateDirectory tracer $ \node ->
      executeRunner tracer node runner

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
