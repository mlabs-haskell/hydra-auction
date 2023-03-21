module EndToEnd.HydraUtils (runningThreeNodesHydra) where

-- Preludes import
import Hydra.Prelude (MonadIO (liftIO), contramap)
import Test.Hydra.Prelude (failAfter, failure, withTempDir)
import Prelude

-- Haskell import
import Data.Foldable (Foldable (toList))

-- Cardano imports
import CardanoNode (
  RunningNode (
    RunningNode,
    networkId,
    nodeSocket
  ),
 )

-- Hydra imports

import Hydra.Chain.Direct (loadChainContext)
import Hydra.Cluster.Faucet (
  Marked (Fuel, Normal),
  publishHydraScriptsAs,
  seedFromFaucet,
  seedFromFaucet_,
 )
import Hydra.Cluster.Fixture (
  Actor (..),
  alice,
  aliceSk,
  bob,
  bobSk,
  carol,
  carolSk,
  cperiod,
 )
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Party (deriveParty)
import HydraNode (
  EndToEndLog (FromFaucet),
  input,
  send,
  waitForNodesConnected,
  withHydraCluster,
 )

runningThreeNodesHydra cardanoNode hydraTracer cont =
  liftIO $
    withTempDir "end-to-end-init-and-close" $ \tmpDir -> do
      let clusterIx = 0

      hydraScriptsTxId <- publishHydraScriptsAs cardanoNode Faucet

      [aliceA, bobA, carolA] <- return [Alice, Bob, Carol]

      aliceKeys@(aliceCardanoVk, _) <- keysFor aliceA
      bobKeys@(bobCardanoVk, _) <- keysFor bobA
      carolKeys@(carolCardanoVk, _) <- keysFor carolA

      let hydraKeys = [aliceSk, bobSk, carolSk]
      parties@[p1, p2, p3] <- return $ map deriveParty hydraKeys

      let cardanoKeys = [aliceKeys, bobKeys, carolKeys]

      let firstNodeId = clusterIx * 3

      chainConfig <- chainConfigFor Alice tmpDir (nodeSocket cardanoNode) [Bob, Carol] cperiod
      chainContext <- loadChainContext chainConfig p1 [p2, p3] hydraScriptsTxId

      withHydraCluster
        hydraTracer
        tmpDir
        (nodeSocket cardanoNode)
        firstNodeId
        cardanoKeys
        hydraKeys
        hydraScriptsTxId
        cperiod
        $ \nodes -> do
          [n1, n2, n3] <- return $ toList nodes
          waitForNodesConnected hydraTracer [n1, n2, n3]

          -- Funds to be used as fuel by Hydra protocol transactions
          let faucetTracer = contramap FromFaucet hydraTracer
          seedFromFaucet_ cardanoNode aliceCardanoVk 200_000_000 Fuel faucetTracer
          seedFromFaucet_ cardanoNode bobCardanoVk 200_000_000 Fuel faucetTracer
          seedFromFaucet_ cardanoNode carolCardanoVk 200_000_000 Fuel faucetTracer

          -- -- TODO remove
          -- seedFromFaucet_ cardanoNode aliceCardanoVk 200_000_000 Normal faucetTracer
          -- seedFromFaucet_ cardanoNode bobCardanoVk 200_000_000 Normal faucetTracer
          -- seedFromFaucet_ cardanoNode carolCardanoVk 200_000_000 Normal faucetTracer

          cont (parties, [n1, n2, n3], chainContext)
