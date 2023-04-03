module EndToEnd.HydraUtils (
  runningThreeNodesDockerComposeHydra,
  filterNonFuelUtxo,
  prepareScriptRegistry,
) where

-- Preludes import
import Hydra.Prelude (MonadIO (liftIO), contramap)
import Prelude

-- Haskell import

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Tracer (stdoutTracer)
import Data.Map qualified as Map
import Network.WebSockets (runClient)
import System.Process (system)

-- Cardano imports

import CardanoNode (
  RunningNode (..),
 )

-- Hydra imports

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry, queryScriptRegistry)
import Hydra.Chain.Direct.Util (isMarkedOutput)
import Hydra.Cluster.Faucet (publishHydraScriptsAs)
import Hydra.Cluster.Fixture qualified as HydraFixture
import HydraNode (HydraClient (..))

-- HydraAuction imports

import Hydra.Cardano.Api (UTxO' (UTxO))
import HydraAuctionUtils.Fixture (
  Actor (..),
  hydraNodeActors,
 )

prepareScriptRegistry :: RunningNode -> IO ScriptRegistry
prepareScriptRegistry node@RunningNode {networkId, nodeSocket} = do
  hydraScriptsTxId <-
    liftIO $ publishHydraScriptsAs node HydraFixture.Faucet
  queryScriptRegistry networkId nodeSocket hydraScriptsTxId

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

filterNonFuelUtxo :: UTxO.UTxO -> UTxO.UTxO
filterNonFuelUtxo =
  UTxO . snd . Map.partition isMarkedOutput . UTxO.toMap
