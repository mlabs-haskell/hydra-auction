module EndToEnd.HydraUtils (
  spinUpHeads,
  runningThreeNodesDockerComposeHydra,
) where

-- Preludes import
import Hydra.Prelude (MonadIO (liftIO), ask, contramap, toList)
import Prelude

-- Haskell import

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Tracer (stdoutTracer)
import Network.WebSockets (runClient)
import System.Environment (setEnv)
import System.Process (system)

-- Hydra imports

import CardanoNode (
  RunningNode (..),
 )
import Hydra.Cluster.Faucet (Marked (..), seedFromFaucet_)
import Hydra.ContestationPeriod (
  ContestationPeriod (UnsafeContestationPeriod),
 )
import HydraNode (EndToEndLog (..), HydraClient (..), waitForNodesConnected, withHydraCluster)
import Test.Hydra.Prelude (withTempDir)

-- HydraAuction imports

import Hydra.Cardano.Api (TxId)
import HydraAuction.Runner (ExecutionContext (MkExecutionContext, node, tracer), HydraAuctionLog (FromHydra), Runner, executeRunner)
import HydraAuctionUtils.BundledData (lookupProtocolParamPath)

-- HydraAuction imports

import HydraAuctionUtils.Fixture (
  Actor (..),
  hydraKeysFor,
  hydraNodeActors,
  keysFor,
 )

spinUpHeads :: Int -> TxId -> (ThreeClients -> Runner ()) -> Runner ()
spinUpHeads clusterIx hydraScriptsTxId cont = do
  liftIO $ do
    hydraDir <- lookupProtocolParamPath
    setEnv "HYDRA_CONFIG_DIR" hydraDir
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

runningThreeNodesDockerComposeHydra ::
  (ThreeClients -> IO b) ->
  IO b
runningThreeNodesDockerComposeHydra cont = do
  _ <- system "./scripts/spin-up-new-devnet.sh"

  -- FIXME: more relaible wait (not sure for what, guess sockets opening)
  threadDelay 2_000_000

  [actor1, actor2, actor3] <- return hydraNodeActors

  runHydraClientN 1 $
    \n1 -> runHydraClientN 2 $
      \n2 -> runHydraClientN 3 $
        \n3 ->
          let threeClients = ((n1, actor1), (n2, actor2), (n3, actor3))
           in finally
                (cont threeClients)
                (system "docker-compose down")
  where
    runHydraClientN n cont' = liftIO $
      runClient "127.0.0.1" (4000 + n) "/history=no" $
        \connection ->
          cont' $
            HydraClient
              { hydraNodeId = n
              , connection = connection
              , tracer = hydraTracerN n
              }
    hydraTracerN n =
      contramap
        (\x -> "Hydra client for node " <> show n <> " :" <> show x)
        stdoutTracer
