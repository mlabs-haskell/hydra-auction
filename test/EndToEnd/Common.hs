{-# LANGUAGE RecordWildCards #-}

module EndToEnd.Common where

import Control.Monad.Reader

import Cardano.Api.UTxO as UTxO
import CardanoClient (
  QueryPoint (QueryTip),
  buildAddress,
  buildScriptAddress,
  queryUTxO,
 )

import Data.Fixed
import Data.Maybe

import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api (
  Data,
  POSIXTime (POSIXTime, getPOSIXTime),
  PubKeyHash (PubKeyHash),
  getValidator,
  toBuiltin,
  toBuiltinData,
  toData,
 )

import System.FilePath ((</>))

import CardanoNode (
  RunningNode (networkId, nodeSocket),
  withCardanoNodeDevnet,
 )

import Hydra.Cardano.Api
import Hydra.Chain.CardanoClient (
  awaitTransaction,
  queryEraHistory,
  queryProtocolParameters,
  queryStakePools,
  querySystemStart,
  submitTransaction,
 )
import Hydra.Chain.Direct.TimeHandle
import Hydra.Cluster.Faucet
import Hydra.Cluster.Fixture
import Hydra.Cluster.Util
import Hydra.Ledger.Cardano.Builder
import Hydra.Logging
import Hydra.Prelude

import HydraAuction.Addresses
import HydraAuction.OnChain
import HydraAuction.Types

import HydraNode

import Test.Hydra.Prelude
import Test.Tasty
import Test.Tasty.Hspec

import System.Directory

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
