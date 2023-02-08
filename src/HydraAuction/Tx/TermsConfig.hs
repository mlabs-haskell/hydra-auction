{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Tx.TermsConfig (TermsConfig (..), termsFromConfig) where

import Prelude

import Control.Applicative (empty)
import Data.Aeson
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (TxIn, toPlutusKeyHash, toPlutusTxOutRef, verificationKeyHash)
import Hydra.Cluster.Fixture (Actor)
import Hydra.Cluster.Util (keysFor)
import HydraAuction.Types
import Plutus.V2.Ledger.Api (POSIXTime (..))

import HydraAuction.OnChain.TestNFT (allowMintingAssetClass)

data TermsConfig = TermsConfig
  { biddingStartPosixSeconds :: Natural
  , biddingEndDelta :: Natural
  , voucherExpiryDelta :: Natural
  , cleanupDelta :: Natural
  , auctionFee :: Natural
  , startingBid :: Natural
  , minimumBidIncrement :: Natural
  }

instance FromJSON TermsConfig where
  parseJSON (Object v) =
    TermsConfig
      <$> v .: "biddingStartPosixSeconds"
      <*> v .: "biddingEndDelta"
      <*> v .: "voucherExpiryDelta"
      <*> v .: "cleanupDelta"
      <*> v .: "auctionFee"
      <*> v .: "startingBid"
      <*> v .: "minimumBidIncrement"
  parseJSON _ = empty

termsFromConfig :: TermsConfig -> Actor -> TxIn -> IO AuctionTerms
termsFromConfig (TermsConfig {..}) seller utxoRef = do
  (sellerVk, _) <- keysFor seller
  let sellerVkHash = toPlutusKeyHash $ verificationKeyHash sellerVk
      -- FIXME
      applyDeltaAndConvert time delta = secondsTo $ naturalToInt time + naturalToInt delta
      secondsTo seconds = POSIXTime $ seconds * 1000
  return $
    AuctionTerms
      { auctionLot = allowMintingAssetClass
      , seller = sellerVkHash
      , delegates = [sellerVkHash]
      , biddingStart =
          secondsTo $ naturalToInt biddingStartPosixSeconds
      , biddingEnd =
          applyDeltaAndConvert biddingStartPosixSeconds biddingEndDelta
      , voucherExpiry =
          applyDeltaAndConvert biddingStartPosixSeconds voucherExpiryDelta
      , cleanup =
          applyDeltaAndConvert biddingStartPosixSeconds cleanupDelta
      , auctionFee = auctionFee
      , startingBid = startingBid
      , minimumBidIncrement = minimumBidIncrement
      , utxoRef = toPlutusTxOutRef utxoRef
      }
