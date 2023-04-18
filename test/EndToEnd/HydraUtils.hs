module EndToEnd.HydraUtils (
  filterNonFuelUtxo,
  prepareScriptRegistry,
  spinUpHeads,
) where

-- Preludes import
import Hydra.Prelude (MonadIO (liftIO), ask, contramap, toList)
import Prelude

-- Haskell import

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Tracer (stdoutTracer)
import Data.Map qualified as Map
import Network.WebSockets (runClient)
import System.Environment (setEnv)
import System.Process (system)

-- Cardano imports

import CardanoNode (
  RunningNode (..),
 )

-- Hydra imports

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry, queryScriptRegistry)
import Hydra.Chain.Direct.Util (isMarkedOutput)
import Hydra.Cluster.Faucet (Marked (..), publishHydraScriptsAs, seedFromFaucet_)
import Hydra.Cluster.Fixture qualified as HydraFixture
import Hydra.ContestationPeriod (
  ContestationPeriod (UnsafeContestationPeriod),
 )
import HydraNode (EndToEndLog (..), HydraClient (..), waitForNodesConnected, withHydraCluster)
import Test.Hydra.Prelude (withTempDir)

-- HydraAuction imports

import Hydra.Cardano.Api (TxId, UTxO' (UTxO))
import HydraAuction.Runner (ExecutionContext (MkExecutionContext, node, tracer), HydraAuctionLog (FromHydra), Runner, executeRunner)
import HydraAuctionUtils.Fixture (
  Actor (..),
  hydraKeysFor,
  hydraNodeActors,
  keysFor,
 )

prepareScriptRegistry :: RunningNode -> IO (TxId, ScriptRegistry)
prepareScriptRegistry node@RunningNode {networkId, nodeSocket} = do
  hydraScriptsTxId <-
    liftIO $ publishHydraScriptsAs node HydraFixture.Faucet
  scriptRegistry <- queryScriptRegistry networkId nodeSocket hydraScriptsTxId
  pure (hydraScriptsTxId, scriptRegistry)

spinUpHeads :: Int -> TxId -> (ThreeClients -> Runner ()) -> Runner ()
spinUpHeads clusterIx hydraScriptsTxId cont = do
  liftIO $ setEnv "HYDRA_CONFIG_DIR" "./data"
  ctx@(MkExecutionContext {node, tracer}) <- ask
  let hydraTracer = contramap FromHydra tracer
  liftIO $
    withTempDir "end-to-end-test" $ \tmpDir -> do
      oscarKeys@(oscarCardanoVk, _) <- keysFor Oscar
      patriciaKeys@(patriciaCardanoVk, _) <- keysFor Patricia
      rupertKeys@(rupertCardanoVk, _) <- keysFor Rupert

      (_, oscarSk) <- hydraKeysFor Oscar
      (_, patriciaSk) <- hydraKeysFor Patricia
      (_, rupertSk) <- hydraKeysFor Rupert

      let cardanoKeys = [oscarKeys, patriciaKeys, rupertKeys]
          hydraKeys = [oscarSk, patriciaSk, rupertSk]

      let firstNodeId = clusterIx * 3
      let contestationPeriod = UnsafeContestationPeriod 2
      withHydraCluster
        hydraTracer
        tmpDir
        (nodeSocket node)
        firstNodeId
        cardanoKeys
        hydraKeys
        hydraScriptsTxId
        contestationPeriod
        $ \nodes -> do
          [n1, n2, n3] <- pure $ toList nodes
          waitForNodesConnected hydraTracer [n1, n2, n3]
          -- Funds to be used as fuel by Hydra protocol transactions
          let faucetTracer = contramap FromFaucet hydraTracer
          seedFromFaucet_ node oscarCardanoVk 100_000_000 Fuel faucetTracer
          seedFromFaucet_ node patriciaCardanoVk 100_000_000 Fuel faucetTracer
          seedFromFaucet_ node rupertCardanoVk 100_000_000 Fuel faucetTracer
          executeRunner ctx $ cont ((n1, Oscar), (n2, Patricia), (n3, Rupert))

type ThreeClients =
  ((HydraClient, Actor), (HydraClient, Actor), (HydraClient, Actor))

filterNonFuelUtxo :: UTxO.UTxO -> UTxO.UTxO
filterNonFuelUtxo =
  UTxO . snd . Map.partition isMarkedOutput . UTxO.toMap
