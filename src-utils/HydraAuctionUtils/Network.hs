module HydraAuctionUtils.Network (runHydraClient) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Tracer (nullTracer)
import Data.Text qualified as Text
import Network.WebSockets (runClient)

-- Hydra imports

import Hydra.Network (Host (..))
import HydraNode (HydraClient (..))

runHydraClient :: Host -> Bool -> (HydraClient -> IO b) -> IO b
runHydraClient host retrieveHistory action = do
  runClient
    (Text.unpack $ hostname host)
    (fromIntegral $ port host)
    path
    cont
  where
    path = "/history=" <> (if retrieveHistory then "yes" else "no")
    cont connection = do
      -- FIXME: use logs
      putStrLn $ "Hydra connection opened for " <> show host
      action $
        HydraClient
          { -- Seems like `hydraNodeId` is used for logging only
            hydraNodeId = -100
          , connection = connection
          , tracer = contramap show nullTracer
          }
