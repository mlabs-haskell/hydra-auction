module EndToEnd.HydraUtils (
  runningThreeNodesDockerComposeHydra,
) where

-- Preludes import
import Hydra.Prelude (MonadIO (liftIO), contramap)
import Prelude

-- Haskell import

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Tracer (stdoutTracer)
import Network.WebSockets (runClient)
import System.Process (system)

-- Hydra imports

import HydraNode (HydraClient (..))

-- HydraAuction imports

import HydraAuctionUtils.Fixture (
  Actor (..),
  hydraNodeActors,
 )

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
