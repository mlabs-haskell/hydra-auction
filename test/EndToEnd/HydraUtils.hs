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
  Marked (Fuel, Normal),
  publishHydraScriptsAs,
  seedFromFaucet_,
 )
import Hydra.Cluster.Fixture (
  Actor (Faucet),
  aliceSk,
  bobSk,
  carolSk,
  cperiod
 )
import Hydra.Party (deriveParty)
import Hydra.Chain.Direct.Util (isMarkedOutput)
import HydraNode (
  EndToEndLog (FromFaucet),
  waitForNodesConnected,
  withHydraCluster,
 )

import HydraAuction.Hydra.Monad (
  MonadHydra(sendCommand), sendCommandAndWaitFor, AwaitedHydraEvent(..))
import HydraAuction.Hydra.Interface (
  HydraCommand(..), HydraEvent (..))
import qualified Cardano.Api.UTxO as UTxO
import HydraAuctionUtils.Fixture as HAFixture (
  Actor(..), keysFor, chainConfigFor)

import Hydra.Cardano.Api (pattern UTxO)
import qualified Data.Map as Map

-- runningThreeNodesHydra ::
--   MonadIO m =>
--   RunningNode
--   -> Tracer IO EndToEndLog
--   -> (([(Hydra.Party.Party, HydraNode.HydraClient, Actor)], Hydra.Chain.Direct.State.ChainContext) -> IO ())
--   -> m ()
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
        chainConfigFor aliceA tmpDir (nodeSocket cardanoNode) [bobA, carolA]
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

          -- Funds to be used as fuel and collaterals by Hydra protocol transactions
          let
            faucetTracer = contramap FromFaucet hydraTracer
            fundActorByVk actorVk = do
              seedFromFaucet_ cardanoNode actorVk 200_000_000 Fuel faucetTracer
              seedFromFaucet_ cardanoNode actorVk 200_000_000 Normal faucetTracer

          mapM_ fundActorByVk (map fst hydraCardanoKeys)

          cont (
            [(p1, n1, aliceA), (p2, n2, bobA), (p3, n3, carolA)],
            chainContext)

commitWithCollateralAdaFor ::
  (MonadHydra m, MonadIO m) => RunningNode -> HAFixture.Actor -> m ()
commitWithCollateralAdaFor node hydraActor = do
  utxo <- liftIO $ do
    (vk, _) <- liftIO $ keysFor hydraActor
    queryUTxOFor (networkId node) (nodeSocket node) QueryTip vk
  -- TODO: monadic query and ensure single utxo
  let tx = UTxO . snd  . Map.partition isMarkedOutput . UTxO.toMap $ utxo
  _ <- sendCommandAndWaitFor (SpecificEvent $ Committed tx) (Commit tx)
  return ()
