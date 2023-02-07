{-# LANGUAGE RecordWildCards #-}

module EndToEnd.Common (Context (..), runScenario, initWallet, getStateDirectory) where

import Control.Monad.Reader
import System.Directory
import System.FilePath ((</>))

import CardanoNode (
  RunningNode,
  withCardanoNodeDevnet,
 )
import Hydra.Cardano.Api
import Hydra.Cluster.Faucet
import Hydra.Cluster.Fixture
import Hydra.Cluster.Util
import Hydra.Logging
import Hydra.Prelude
import HydraNode

data Context = MkContext
  { tracer :: Tracer IO EndToEndLog
  , node :: RunningNode
  }

type Scenario a = ReaderT Context IO a

getStateDirectory :: IO FilePath
getStateDirectory = do
  currentDirectory <- getCurrentDirectory
  let stateDirectory = currentDirectory </> "node-state"
  createDirectoryIfMissing True stateDirectory
  pure stateDirectory

runScenario :: Scenario () -> IO ()
runScenario scenario = do
  stateDirectory <- getStateDirectory
  withFile (stateDirectory </> "test.log") ReadWriteMode $ \h ->
    withTracerOutputTo h "Tracer" $ \tracer ->
      withCardanoNodeDevnet
        (contramap FromCardanoNode tracer)
        stateDirectory
        $ \node -> runReaderT scenario (MkContext tracer node)

initWallet :: Actor -> Lovelace -> Scenario ()
initWallet actor amount = do
  MkContext {..} <- ask
  liftIO $ do
    (vk, _) <- keysFor actor
    seedFromFaucet_
      node
      vk
      amount
      Normal
      (contramap FromFaucet tracer)
