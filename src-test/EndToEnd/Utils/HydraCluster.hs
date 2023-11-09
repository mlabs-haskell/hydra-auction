module EndToEnd.Utils.HydraCluster (
  withManualHydraCluster,
  withDockerComposeCluster,
) where

-- Preludes import
import Hydra.Prelude (toList)
import HydraAuctionUtils.Prelude

-- Haskell import

-- import Control.Concurrent.Async (mapConcurrently)
import Control.Tracer (nullTracer)
import Data.Map qualified as Map
import System.Environment (setEnv)
import System.Process (system)

-- Hydra imports

import CardanoNode (
  RunningNode (..),
 )
import Hydra.Cardano.Api (TxId)
import Hydra.ContestationPeriod (
  ContestationPeriod (UnsafeContestationPeriod),
 )
import Hydra.Network (Host (..))
import HydraNode (HydraClient (..), waitForNodesConnected, withHydraCluster)
import Test.Hydra.Prelude (withTempDir)

-- HydraAuction imports
import HydraAuctionUtils.BundledData (lookupProtocolParamPath)
import HydraAuctionUtils.Fixture (
  Actor (..),
  ActorKind (..),
  actorsByKind,
  hydraKeysFor,
  keysFor,
 )
import HydraAuctionUtils.Hydra.Interface (
  HydraConnectionConfig (..),
  HydraProtocol,
 )
import HydraAuctionUtils.L1.Runner (
  ExecutionContext (..),
  L1Runner,
  dockerNode,
  executeL1Runner,
  executeL1RunnerWithNode,
  -- executeTestL1Runner,
  withActor,
 )
import HydraAuctionUtils.Tx.Common (transferAda)
import HydraAuctionUtils.WebSockets.Client (
  RealProtocolClient (MkRealProtocolClient),
  withProtocolClient,
 )

-- Ways to spin up Hydra cluster

-- This function will set the HYDRA_CONFIG_DIR env var locally
-- This is required so the hydra nodes pick up on the correct protocol-parameters.json
-- file.
-- FIXME: remove all of this: HYDRA_CONFIG_DIR hack, Hydra internal util usage,
-- and overlall separation of "spin-up" functions with not-clear invariants
withManualHydraCluster :: HasCallStack => TxId -> ([RealProtocolClient HydraProtocol] -> L1Runner ()) -> L1Runner ()
withManualHydraCluster hydraScriptsTxId cont = do
  liftIO $ do
    hydraDir <- lookupProtocolParamPath
    setEnv "HYDRA_CONFIG_DIR" hydraDir
  ctx@(MkExecutionContext {node}) <- ask
  seedHydraNodes
  liftIO $
    withTempDir "end-to-end-test" $ \tmpDir -> do
      cardanoKeys <- mapM keysFor actors
      hydraSks <- mapM (fmap snd . hydraKeysFor) actors
      let firstNodeId = 1 -- Other will probably not work
      let contestationPeriod = UnsafeContestationPeriod 2
      withHydraCluster
        nullTracer
        tmpDir
        (nodeSocket node)
        firstNodeId
        cardanoKeys
        hydraSks
        hydraScriptsTxId
        contestationPeriod
        $ \nodes -> do
          waitForNodesConnected nullTracer $ toList nodes
          executeL1Runner ctx $ do
            cont $ map clientFromNode $ toList nodes
  where
    actors = (Map.!) actorsByKind HydraNodeActor
    initActor actor = withActor Faucet $ transferAda actor 100_000_000
    seedHydraNodes = mapM_ initActor actors
    clientFromNode node =
      MkRealProtocolClient
        (Host "127.0.0.1" (fromIntegral $ 4_000 + hydraNodeId node))
        $ connection node

withDockerComposeCluster ::
  ([RealProtocolClient HydraProtocol] -> L1Runner b) ->
  IO b
withDockerComposeCluster cont = do
  _ <- system "./scripts/spin-up-new-devnet.sh 0"
  withNClients 3 (action . reverse)
  where
    withHydraClientN n =
      withProtocolClient
        (Host "127.0.0.1" (fromIntegral $ 4000 + n))
        (MkHydraConnectionConfig {retrieveHistory = False})
    withNClients' clientsAcc n cont' =
      if length clientsAcc == n
        then cont' clientsAcc
        else withHydraClientN (length clientsAcc + 1) $
          \client -> withNClients' (client : clientsAcc) n cont'
    withNClients = withNClients' []
    action nodes = do
      executeL1RunnerWithNode dockerNode (cont nodes)
