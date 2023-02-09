module CliConfig (
  AuctionName (..),
  AuctionTermsConfig (..),
  AuctionTermsDynamic (..),
  constructTermsDynamic,
  readAuctionTermsConfig,
  readAuctionTermsDynamic,
  readAuctionTerms,
  writeAuctionTermsConfig,
  writeAuctionTermsDynamic,
  configToAuctionTerms,
) where

import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Prelude

import HydraAuction.Tx.TermsConfig
import HydraAuction.Types

import System.Directory
import System.FilePath ((<.>), (</>))

-- =============================================================================
-- Auction config and state directories
newtype AuctionName = AuctionName String
  deriving stock (Prelude.Show, Prelude.Eq)

getRelativeDirectory :: FilePath -> IO FilePath
getRelativeDirectory filepath = do
  currentDirectory <- getCurrentDirectory
  let dir = currentDirectory </> filepath
  createDirectoryIfMissing True dir
  pure dir

getAuctionConfigDirectory :: IO FilePath
getAuctionConfigDirectory =
  getRelativeDirectory $ "example" </> "auction-config"

getAuctionStateDirectory :: IO FilePath
getAuctionStateDirectory =
  getRelativeDirectory "auction-state"

-- =============================================================================
-- Read/write auction config files
getAuctionConfigFilePath :: AuctionName -> IO FilePath
getAuctionConfigFilePath (AuctionName auctionName) = do
  auctionConfigDir <- getAuctionConfigDirectory
  pure $ auctionConfigDir </> auctionName <.> "json"

readAuctionTermsConfig :: AuctionName -> IO (Maybe AuctionTermsConfig)
readAuctionTermsConfig auctionName = do
  filepath <- getAuctionConfigFilePath auctionName
  Aeson.decode . LBS.fromStrict <$> BS.readFile filepath

writeAuctionTermsConfig :: AuctionName -> AuctionTermsConfig -> IO ()
writeAuctionTermsConfig auctionName config = do
  filepath <- getAuctionConfigFilePath auctionName
  BS.writeFile filepath . LBS.toStrict . Aeson.encode $ config

-- =============================================================================
-- Read/write auction dynamic files

getAuctionDynamicFilePath :: AuctionName -> IO FilePath
getAuctionDynamicFilePath (AuctionName auctionName) = do
  auctionDynamicDir <- getAuctionStateDirectory
  pure $ auctionDynamicDir </> auctionName <.> "json"

readAuctionTermsDynamic :: AuctionName -> IO (Maybe AuctionTermsDynamic)
readAuctionTermsDynamic auctionName = do
  filepath <- getAuctionDynamicFilePath auctionName
  Aeson.decode . LBS.fromStrict <$> BS.readFile filepath

writeAuctionTermsDynamic :: AuctionName -> AuctionTermsDynamic -> IO ()
writeAuctionTermsDynamic auctionName config = do
  filepath <- getAuctionDynamicFilePath auctionName
  BS.writeFile filepath . LBS.toStrict . Aeson.encode $ config

-- =============================================================================
-- Read full auction terms

readAuctionTerms :: AuctionName -> IO (Maybe AuctionTerms)
readAuctionTerms auctionName = runMaybeT $ do
  dynamicParams <- MaybeT $ readAuctionTermsDynamic auctionName
  config <- MaybeT $ readAuctionTermsConfig auctionName
  pure $ configToAuctionTerms config dynamicParams
