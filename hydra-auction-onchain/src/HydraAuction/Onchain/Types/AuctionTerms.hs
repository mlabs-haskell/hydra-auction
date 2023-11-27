module HydraAuction.Onchain.Types.AuctionTerms (
  AuctionTerms (..),
  minAuctionFee,
  totalAuctionFees,
  validateAuctionTerms,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusTx qualified

import HydraAuction.Error.Types.AuctionTerms (AuctionTerms'Error (..))
import HydraAuction.Onchain.Lib.Error (eCode, err)

data AuctionTerms = AuctionTerms
  { at'AuctionLot :: AssetClass
  -- ^ NFT being sold in the auction.
  , at'SellerPkh :: PubKeyHash
  -- ^ Seller's pubkey hash, which will receive
  -- the proceeds of the auction (minus fees)
  -- if the auction lot is purchased,
  -- or reclaim the auction lot if it isn't.
  , at'SellerVk :: BuiltinByteString
  -- ^ Seller's verification key, used to control
  -- which bidders receive authorization to participate in the auction.
  , at'Delegates :: [PubKeyHash]
  -- ^ Group of delegates authorized to run the L2 bidding process.
  , at'BiddingStart :: POSIXTime
  -- ^ Start time of the bidding period.
  , at'BiddingEnd :: POSIXTime
  -- ^ End time of the bidding period.
  , at'PurchaseDeadline :: POSIXTime
  -- ^ Time by which the winning bidder can buy the auction lot.
  -- At and after this time, the winning bidder forfeits its bidder deposit
  -- if the auction lot has not been purchased.
  , at'Cleanup :: POSIXTime
  -- ^ Time at and after  which the remaining utxos in the auction
  -- can be unconditionally cleaned up, returning all tokens
  -- in those utxos to their original owners before the auction.
  , at'AuctionFeePerDelegate :: Integer
  -- ^ Fee portion that each delegate will receieve from
  -- the proceeds of the auction, whether the auction lot
  -- is purchased by a bidder or reclaimed by the seller.
  , at'StartingBid :: Integer
  -- ^ Bids cannot be lower than this number.
  , at'MinBidIncrement :: Integer
  -- ^ New bids can only supersede the standing bid if they exceed it
  -- by this increment.
  , at'MinDepositAmount :: Integer
  -- ^ Minimal amount of ADA that the seller requests
  -- each bidder to place as a bidder deposit for the auction.
  -- This is only enforced off-chain at the seller's discretion.
  }

--
-- -------------------------------------------------------------------------
-- Validation
-- -------------------------------------------------------------------------

{-# INLINEABLE validateAuctionTerms #-}
validateAuctionTerms ::
  AuctionTerms ->
  Bool
validateAuctionTerms aTerms@AuctionTerms {..} =
  --
  -- (AT01) The seller pubkey hash corresponds to the seller verification key.
  -- Note: this check only becomes possible on-chain in Plutus V3.
  -- https://github.com/input-output-hk/plutus/pull/5431
  -- (at'SellerPkh == PubKeyHash (blake2b_224 at'SellerVk)) &&
  -- `err` $(eCode AuctionTerms'Error'SellerVkPkhMismatch)
  --
  -- (AT02) Bidding ends after it the bidding start time.
  (at'BiddingStart < at'BiddingEnd)
    `err` $(eCode AuctionTerms'Error'BiddingStartNotBeforeBiddingEnd)
    --
    -- (AT03) The purchase deadline occurs after bidding ends.
    && (at'BiddingEnd < at'PurchaseDeadline)
    `err` $(eCode AuctionTerms'Error'BiddingEndNotBeforePurchaseDeadline)
    --
    -- (AT04) Cleanup happens after the purchase deadline,
    -- so that the seller can claim the winning bidder's deposit
    -- if the auction lot is not sold
    && (at'PurchaseDeadline < at'Cleanup)
    `err` $(eCode AuctionTerms'Error'PurchaseDeadlineNotBeforeCleanup)
    --
    -- (AT05) New bids must be larger than the standing bid.
    && (at'MinBidIncrement > 0)
    `err` $(eCode AuctionTerms'Error'NonPositiveMinBidIncrement)
    --
    -- (AT06) The auction fee for each delegate must contain
    -- the min 2 ADA for the utxos that will be sent to the delegates
    -- during fee distribution.
    && (at'StartingBid > totalAuctionFees aTerms)
    `err` $(eCode AuctionTerms'Error'InvalidStartingBid)
    --
    -- (AT07) The auction fees for all delegates must be covered by
    -- the starting bid.
    && (at'AuctionFeePerDelegate > minAuctionFee)
    `err` $(eCode AuctionTerms'Error'InvalidAuctionFeePerDelegate)
    --
    -- (AT08) There must be at least one delegate.
    && (length at'Delegates > 0)
    `err` $(eCode AuctionTerms'Error'NoDelegates)

{-# INLINEABLE minAuctionFee #-}
minAuctionFee :: Integer
minAuctionFee = 2_500_00

{-# INLINEABLE totalAuctionFees #-}
totalAuctionFees :: AuctionTerms -> Integer
totalAuctionFees AuctionTerms {..} =
  at'AuctionFeePerDelegate * length at'Delegates

-- -------------------------------------------------------------------------
-- Plutus instances
-- -------------------------------------------------------------------------
PlutusTx.unstableMakeIsData ''AuctionTerms

instance Eq AuctionTerms where
  (AuctionTerms x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)
    == (AuctionTerms y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) =
      x1 == y1
        && x2 == y2
        && x3 == y3
        && x4 == y4
        && x5 == y5
        && x6 == y6
        && x7 == y7
        && x8 == y8
        && x9 == y9
        && x10 == y10
        && x11 == y11
        && x12 == y12
