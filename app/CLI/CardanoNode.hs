module CLI.CardanoNode (runCardanoNode, getCardanoNode) where

-- Prelude imports
import Hydra.Prelude (contramap)
import Prelude

-- Haskell imports

import Control.Concurrent.MVar
import System.FilePath ((</>))

-- Cardano node imports
import Cardano.Api (NetworkId (..))

-- Hydra imports
import CardanoNode (
  RunningNode (
    RunningNode,
    networkId,
    nodeSocket
  ),
  withCardanoNodeDevnet,
 )
import Hydra.Cardano.Api (NetworkMagic (NetworkMagic))
import Hydra.Logging (Tracer)

-- Hydra auction imports
import HydraAuction.Runner (EndToEndLog (..))

-- Hydra auction CLI imports
import CLI.Config (
  DirectoryKind (..),
  getAuctionDirectory,
 )

runCardanoNode :: Tracer IO EndToEndLog -> IO ()
runCardanoNode tracer = do
  mv <- newEmptyMVar
  stateDirectory <- getAuctionDirectory AuctionStateCardanoNode
  withCardanoNodeDevnet
    (contramap FromCardanoNode tracer)
    stateDirectory
    $ \_ -> takeMVar mv

getCardanoNode :: IO RunningNode
getCardanoNode = do
  stateDirectory <- getAuctionDirectory AuctionStateCardanoNode
  return $
    RunningNode
      { nodeSocket = stateDirectory </> "node.socket"
      , networkId = Testnet $ NetworkMagic 42
      }
