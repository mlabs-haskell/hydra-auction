{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Tx.TermsConfig (
  AuctionTermsConfig (..),
  AuctionTermsDynamic (..),
  constructTermsDynamic,
  configToAuctionTerms,
) where

import PlutusTx.Prelude (emptyByteString)
import Prelude

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Hydra.Cardano.Api (TxIn, toPlutusKeyHash, toPlutusTxOutRef, verificationKeyHash)
import Hydra.Cluster.Fixture (Actor)
import Hydra.Cluster.Util (keysFor)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Time (POSIXTime (..))
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol (..))
import Plutus.V2.Ledger.Contexts (TxOutRef)

import HydraAuction.OnChain.TestNFT
import HydraAuction.PlutusOrphans ()
import HydraAuction.Tx.Common (currentTimeSeconds)
import HydraAuction.Types (AuctionTerms (..), Natural)

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
  { configAuctionLot :: !AssetClass
  , -- Storing Actor, not only PubKeyHash, is required to simplify CLI actions on seller behalf
    configSellerActor :: !Actor
  , configDelegates :: ![PubKeyHash]
  , configUtxoNonce :: !TxOutRef
  , configAnnouncementTime :: !POSIXTime
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance ToJSON AuctionTermsDynamic

instance FromJSON AuctionTermsDynamic

getActorVkHash :: Actor -> IO PubKeyHash
getActorVkHash actor = do
  (actorVk, _) <- keysFor actor
  return $ toPlutusKeyHash $ verificationKeyHash actorVk

constructTermsDynamic :: Actor -> TxIn -> IO AuctionTermsDynamic
constructTermsDynamic sellerActor utxoNonce = do
  currentTimeSeconds' <- currentTimeSeconds
  sellerVkHash <- getActorVkHash sellerActor
  return $
    AuctionTermsDynamic
      { configAuctionLot = allowMintingAssetClass
      , configSellerActor = sellerActor
      , -- FIXME
        configDelegates = [sellerVkHash]
      , configUtxoNonce = toPlutusTxOutRef utxoNonce
      , configAnnouncementTime = POSIXTime $ currentTimeSeconds' * 1000
      }

configToAuctionTerms ::
  AuctionTermsConfig ->
  AuctionTermsDynamic ->
  IO AuctionTerms
configToAuctionTerms AuctionTermsConfig {..} AuctionTermsDynamic {..} = do
  sellerVkHash <- getActorVkHash configSellerActor
  return $
    AuctionTerms
      { auctionLot = configAuctionLot
      , seller = sellerVkHash
      , -- FUTURE FIXME
        hydraHeadId = CurrencySymbol emptyByteString
      , delegates = configDelegates
      , biddingStart = toAbsTime configDiffBiddingStart
      , biddingEnd = toAbsTime configDiffBiddingEnd
      , voucherExpiry = toAbsTime configDiffVoucherExpiry
      , cleanup = toAbsTime configDiffCleanup
      , auctionFee = configAuctionFee
      , startingBid = configStartingBid
      , minimumBidIncrement = configMinimumBidIncrement
      , utxoNonce = configUtxoNonce
      }
  where
    (POSIXTime announcementTime) = configAnnouncementTime
    -- Converting seconds to milliseconds
    toAbsTime n = POSIXTime $ announcementTime + (n * 1000)
