{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Runner (
  Runner,
  executeRunner,
  StateDirectory,
  ExecutionContext (..),
  defStateDirectory,
  tmpStateDirectory,
  initWallet,
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
  Bool (True),
  Contravariant (contramap),
  FilePath,
  Functor,
  IO,
  IOMode (ReadWriteMode),
  Monad,
  MonadIO (..),
  MonadReader (ask),
  ReaderT (..),
  String,
  withFile,
  ($),
 )

import HydraNode (EndToEndLog (FromCardanoNode, FromFaucet))

import System.Directory (
  createDirectoryIfMissing,
  getCurrentDirectory,
 )
import System.FilePath ((</>))

import Test.Hydra.Prelude (withTempDir)

data ExecutionContext = MkExecutionContext
  { tracer :: !(Tracer IO EndToEndLog)
  , node :: !RunningNode
  }

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

executeRunner :: StateDirectory -> Runner () -> IO ()
executeRunner MkStateDirectory {stateDirectory} runner = do
  withFile (stateDirectory </> "test.log") ReadWriteMode $ \h ->
    withTracerOutputTo h "Tracer" $ \tracer ->
      withCardanoNodeDevnet
        (contramap FromCardanoNode tracer)
        stateDirectory
        $ \node ->
          runReaderT
            (run runner)
            (MkExecutionContext tracer node)

newtype StateDirectory = MkStateDirectory
  {stateDirectory :: FilePath}

defStateDirectory :: IO StateDirectory
defStateDirectory = do
  currentDirectory <- getCurrentDirectory
  let stateDirectory = currentDirectory </> "node-state"
  createDirectoryIfMissing True stateDirectory
  pure $ MkStateDirectory stateDirectory

tmpStateDirectory :: String -> Runner () -> IO ()
tmpStateDirectory dir runner = do
  withTempDir dir $ \tmpDir -> do
    executeRunner (MkStateDirectory tmpDir) runner

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
