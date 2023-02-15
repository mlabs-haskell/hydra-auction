module CLI.CardanoNode (runCardanoNode, getCardanoNode) where

-- Prelude imports
import Hydra.Prelude (contramap)
import Prelude

-- Haskell imports
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
import HydraNode (
  EndToEndLog (
    FromCardanoNode
  ),
 )

-- Hydra auction CLI imports
import CLI.Config (
  DirectoryKind (..),
  getAuctionDirectory,
 )

runCardanoNode :: Tracer IO EndToEndLog -> IO ()
runCardanoNode tracer = do
  stateDirectory <- getAuctionDirectory AuctionStateCardanoNode
  withCardanoNodeDevnet
    (contramap FromCardanoNode tracer)
    stateDirectory
    $ \_ ->
      error "Not implemented: RunCardanoNode"

getCardanoNode :: IO RunningNode
getCardanoNode = do
  stateDirectory <- getAuctionDirectory AuctionStateCardanoNode
  return $
    RunningNode
      { nodeSocket = stateDirectory </> "node.socket"
      , networkId = Testnet $ NetworkMagic 42
      }
