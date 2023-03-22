module EndToEnd.HydraUtils (runningThreeNodesHydra, commitWithCollateralAdaFor) where

-- Preludes import
-- Preludes import
import Hydra.Prelude (MonadIO (liftIO), contramap, MonadState (put))
import Test.Hydra.Prelude (withTempDir)
import Prelude

-- Haskell import
import Data.Foldable (Foldable (toList))

-- Cardano imports
import CardanoNode (
  RunningNode (
    networkId,
    nodeSocket
  ),

 )
import CardanoClient (queryUTxOFor, QueryPoint(QueryTip))


-- Hydra imports

import Hydra.Chain.Direct (loadChainContext)
import Hydra.Cluster.Faucet (
  Marked (Fuel),
  publishHydraScriptsAs,
  seedFromFaucet_,
 )
import Hydra.Cluster.Fixture (
  Actor (..),
  aliceSk,
  bobSk,
  carolSk,
  cperiod,
 )
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Party (deriveParty)
import HydraNode (
  EndToEndLog (FromFaucet),
  waitForNodesConnected,
  withHydraCluster,
 )
import HydraAuction.Hydra.Monad (
  MonadHydra(sendCommand), sendCommandAndWaitFor, AwaitedHydraEvent(..))
import HydraAuction.Hydra.Interface (
  HydraCommand(..), HydraEventKind (..), HydraEvent (..))

runningThreeNodesHydra cardanoNode hydraTracer cont =
  liftIO $
    withTempDir "hydra-test-node-dir" $ \tmpDir -> do
      let firstNodeId = 0
      hydraScriptsTxId <- publishHydraScriptsAs cardanoNode Faucet

      hydraCardanoActors@[aliceA, bobA, carolA] <- return [Alice, Bob, Carol]

      let hydraKeys = [aliceSk, bobSk, carolSk]
      [p1, p2, p3] <- return $ map deriveParty hydraKeys

      hydraCardanoKeys <- mapM keysFor hydraCardanoActors

      chainConfig <-
        chainConfigFor aliceA tmpDir (nodeSocket cardanoNode) [bobA, carolA] cperiod
      chainContext <-
        loadChainContext chainConfig p1 [p2, p3] hydraScriptsTxId

      withHydraCluster
        hydraTracer
        tmpDir
        (nodeSocket cardanoNode)
        firstNodeId
        hydraCardanoKeys
        hydraKeys
        hydraScriptsTxId
        cperiod
        $ \nodes -> do
          [n1, n2, n3] <- return $ toList nodes
          waitForNodesConnected hydraTracer [n1, n2, n3]

          -- Funds to be used as fuel by Hydra protocol transactions
          let
            faucetTracer = contramap FromFaucet hydraTracer
            fundActorByVk actorVk =
              seedFromFaucet_ cardanoNode actorVk 200_000_000 Fuel faucetTracer

          mapM_ fundActorByVk (map fst hydraCardanoKeys)

          cont (
            [(p1, n1, aliceA), (p2, n2, bobA), (p3, n3, carolA)],
            chainContext)

commitWithCollateralAdaFor ::
  (MonadHydra m, MonadIO m) => RunningNode -> Actor -> m ()
commitWithCollateralAdaFor node hydraActor = do
  tx <- liftIO $ do
    (vk, _) <- liftIO $ keysFor hydraActor
    queryUTxOFor (networkId node) (nodeSocket node) QueryTip vk
  liftIO $ putStrLn $ "TX Is: " <> show tx
  _ <- sendCommandAndWaitFor (SpecificKind CommittedKind) (Commit tx)
  return ()
