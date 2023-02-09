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
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Hydra.Prelude (liftIO)
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

data DirectoryKind = AuctionConfig | AuctionState

getAuctionDirectory :: DirectoryKind -> IO FilePath
getAuctionDirectory kind =
  getRelativeDirectory path
  where
    path = case kind of
      AuctionConfig -> "example" </> "auction-config"
      AuctionState -> "auction-state"

-- =============================================================================
-- Common JSON read/write code

toJsonFileName :: DirectoryKind -> AuctionName -> IO FilePath
toJsonFileName dirKind (AuctionName auctionName) = do
  directory <- getAuctionDirectory dirKind
  return $ directory </> auctionName <.> "json"

readJsonFromPath :: (FromJSON a) => DirectoryKind -> AuctionName -> IO (Maybe a)
readJsonFromPath dirKind auctionName = do
  filename <- toJsonFileName dirKind auctionName
  Aeson.decode . LBS.fromStrict <$> BS.readFile filename

writeJsonToPath :: (ToJSON a) => DirectoryKind -> AuctionName -> a -> IO ()
writeJsonToPath dirKind auctionName config = do
  filename <- toJsonFileName dirKind auctionName
  (BS.writeFile filename . LBS.toStrict . Aeson.encode) config

-- =============================================================================
-- Read/write auction config files

readAuctionTermsConfig :: AuctionName -> IO (Maybe AuctionTermsConfig)
readAuctionTermsConfig = readJsonFromPath AuctionConfig

writeAuctionTermsConfig :: AuctionName -> AuctionTermsConfig -> IO ()
writeAuctionTermsConfig = writeJsonToPath AuctionConfig

readAuctionTermsDynamic :: AuctionName -> IO (Maybe AuctionTermsDynamic)
readAuctionTermsDynamic = readJsonFromPath AuctionState

writeAuctionTermsDynamic :: AuctionName -> AuctionTermsDynamic -> IO ()
writeAuctionTermsDynamic = writeJsonToPath AuctionState

-- =============================================================================
-- Read full auction terms

readAuctionTerms :: AuctionName -> IO (Maybe AuctionTerms)
readAuctionTerms auctionName = runMaybeT $ do
  dynamicParams <- MaybeT $ readAuctionTermsDynamic auctionName
  config <- MaybeT $ readAuctionTermsConfig auctionName
  liftIO $ configToAuctionTerms config dynamicParams
