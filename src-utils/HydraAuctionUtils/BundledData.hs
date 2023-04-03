module HydraAuctionUtils.BundledData (
  readDataFile,
  readHydraNodeProtocolParams,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Paths_hydra_auction qualified as Pkg
import System.FilePath ((</>))

-- Hydra imports
import Hydra.Cardano.Api (ProtocolParameters)

-- | Lookup a config file similar reading a file from disk.
readDataFile :: FilePath -> IO BS.ByteString
readDataFile source = do
  filename <- Pkg.getDataFileName ("data" </> source)
  BS.readFile filename

readHydraNodeProtocolParams :: IO ProtocolParameters
readHydraNodeProtocolParams = do
  bytes <- readDataFile "devnet-source/protocol-parameters.json"
  case Aeson.eitherDecodeStrict bytes of
    Left errorMsg ->
      fail $
        "Cannot decode protocol-parameters.json, error: " <> show errorMsg
    Right protocolParams -> return protocolParams