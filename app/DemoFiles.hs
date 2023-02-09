{-# LANGUAGE RecordWildCards #-}

module DemoFiles where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Prelude

import GHC.Generics (Generic)
import HydraAuction.Types
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Time (POSIXTime (..))
import Plutus.V1.Ledger.Value (AssetClass)
import Plutus.V2.Ledger.Contexts (TxOutRef)

import System.Directory
import System.FilePath ((<.>), (</>))

data AuctionTermsConfig = AuctionTermsConfig
  { configDiffBiddingStart :: !Integer
  , configDiffBiddingEnd :: !Integer
  , configDiffVoucherExpiry :: !Integer
  , configDiffCleanup :: !Integer
  , configAuctionFee :: !Natural
  , configStartingBid :: !Natural
  , configMinimumBidIncrement :: !Natural
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance ToJSON AuctionTermsConfig

instance FromJSON AuctionTermsConfig

data AuctionTermsDynamic = AuctionTermsDynamic
  { configAuctionLot :: AssetClass
  , configSeller :: !PubKeyHash
  , configDelegates :: ![PubKeyHash]
  , configUtxoRef :: !TxOutRef
  , configAnnouncementTime :: !POSIXTime
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

configToAuctionTerms ::
  AuctionTermsConfig ->
  AuctionTermsDynamic ->
  AuctionTerms
configToAuctionTerms AuctionTermsConfig {..} AuctionTermsDynamic {..} =
  AuctionTerms
    { auctionLot = configAuctionLot
    , seller = configSeller
    , delegates = configDelegates
    , biddingStart = toAbsTime configDiffBiddingStart * 1000
    , biddingEnd = toAbsTime configDiffBiddingEnd * 1000
    , voucherExpiry = toAbsTime configDiffVoucherExpiry * 1000
    , cleanup = toAbsTime configDiffCleanup * 1000
    , auctionFee = configAuctionFee
    , startingBid = configStartingBid
    , minimumBidIncrement = configMinimumBidIncrement
    , utxoRef = configUtxoRef
    }
  where
    (POSIXTime announcementTime) = configAnnouncementTime
    toAbsTime n = POSIXTime $ announcementTime + n

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
