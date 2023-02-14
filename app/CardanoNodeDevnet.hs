module CardanoNodeDevnet (runCardanoNode, getCardanoNode) where

import Prelude

import System.FilePath ((</>))

import Cardano.Api (NetworkId (..))
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
import Hydra.Prelude (contramap)
import HydraNode (
  EndToEndLog (
    FromCardanoNode
  ),
 )

import CliConfig (
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
