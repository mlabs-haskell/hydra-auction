module CLI.Config (
  AuctionName (..),
  DirectoryKind (..),
  CliEnhancedAuctionTerms (..),
  readCliEnhancedAuctionTerms,
  getAuctionDirectory,
  readAuctionTermsConfig,
  readAuctionTermsDynamic,
  readAuctionTerms,
  writeAuctionTermsConfig,
  writeAuctionTermsDynamic,
  configToAuctionTerms,
) where

-- Prelude imports
import Hydra.Prelude (liftIO)
import Prelude

-- Haskell imports
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((<.>), (</>))

-- Hydra auction imports
import HydraAuction.Fixture (Actor (..))
import HydraAuction.Tx.TermsConfig (
  AuctionTermsConfig,
  AuctionTermsDynamic (..),
  configToAuctionTerms,
 )
import HydraAuction.Types (AuctionTerms)

-- =============================================================================
-- Auction config and state directories
newtype AuctionName = AuctionName String
  deriving newtype (Prelude.Show, Prelude.Eq)

getRelativeDirectory :: FilePath -> IO FilePath
getRelativeDirectory filepath = do
  currentDirectory <- getCurrentDirectory
  let dir = currentDirectory </> filepath
  createDirectoryIfMissing True dir
  pure dir

data DirectoryKind = AuctionConfig | AuctionStateDynamic | AuctionStateCardanoNode

getAuctionDirectory :: DirectoryKind -> IO FilePath
getAuctionDirectory kind =
  getRelativeDirectory path
  where
    path = case kind of
      AuctionConfig -> "example" </> "auction-config"
      AuctionStateDynamic -> "auction-state" </> "dynamic"
      AuctionStateCardanoNode -> "auction-state" </> "cardano-node"

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
readAuctionTermsDynamic = readJsonFromPath AuctionStateDynamic

writeAuctionTermsDynamic :: AuctionName -> AuctionTermsDynamic -> IO ()
writeAuctionTermsDynamic = writeJsonToPath AuctionStateDynamic

-- =============================================================================
-- Read full auction terms

data CliEnhancedAuctionTerms = CliEnhancedAuctionTerms
  { -- Storing Actor, not only PubKeyHash, is required to simplify CLI actions on seller behalf
    terms :: AuctionTerms
  , sellerActor :: Actor
  }

readCliEnhancedAuctionTerms :: AuctionName -> IO (Maybe CliEnhancedAuctionTerms)
readCliEnhancedAuctionTerms auctionName = runMaybeT $ do
  dynamicParams <- MaybeT $ readAuctionTermsDynamic auctionName
  config <- MaybeT $ readAuctionTermsConfig auctionName
  terms <- liftIO $ configToAuctionTerms config dynamicParams
  return $
    CliEnhancedAuctionTerms
      { terms = terms
      , sellerActor = configSellerActor dynamicParams
      }

readAuctionTerms :: AuctionName -> IO (Maybe AuctionTerms)
readAuctionTerms name = do
  e <- readCliEnhancedAuctionTerms name
  return $ terms <$> e
