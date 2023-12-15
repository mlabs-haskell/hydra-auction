module HydraAuction.Offchain.Types.AuctionTerms (
  AuctionTerms (..),
  AuctionTerms'Error (..),
  totalAuctionFees,
  validateAuctionTerms,
) where

import GHC.Generics (Generic)
import Prelude

import Data.Time.Clock (UTCTime)
import Data.Validation (Validation)

import Cardano.Api.Shelley (
  AssetId (..),
  Lovelace (..),
 )

import HydraAuction.Error.Types.AuctionTerms (
  AuctionTerms'Error (..),
 )
import HydraAuction.Offchain.Lib.Crypto (
  Hash,
  Key (verificationKeyHash),
  PaymentKey,
  VerificationKey,
 )
import HydraAuction.Offchain.Lib.Validation (err)

data AuctionTerms = AuctionTerms
  { at'AuctionLot :: !AssetId
  -- ^ NFT being sold in the auction.
  , at'SellerPkh :: !(Hash PaymentKey)
  -- ^ Seller's pubkey hash, which will receive
  -- the proceeds of the auction (minus fees)
  -- if the auction lot is purchased,
  -- or reclaim the auction lot if it isn't.
  , at'SellerVk :: !(VerificationKey PaymentKey)
  -- ^ Seller's verification key, used to control
  -- which bidders receive authorization to participate in the auction.
  , at'Delegates :: ![Hash PaymentKey]
  -- ^ Group of delegates authorized to run the L2 bidding process.
  , at'BiddingStart :: !UTCTime
  -- ^ Start time of the bidding period.
  , at'BiddingEnd :: !UTCTime
  -- ^ End time of the bidding period.
  , at'PurchaseDeadline :: !UTCTime
  -- ^ Time by which the winning bidder can buy the auction lot.
  -- At and after this time, the winning bidder forfeits its bidder deposit
  -- if the auction lot has not been purchased.
  , at'Cleanup :: !UTCTime
  -- ^ Time at and after  which the remaining utxos in the auction
  -- can be unconditionally cleaned up, returning all tokens
  -- in those utxos to their original owners before the auction.
  , at'AuctionFeePerDelegate :: !Lovelace
  -- ^ Fee portion that each delegate will receieve from
  -- the proceeds of the auction, whether the auction lot
  -- is purchased by a bidder or reclaimed by the seller.
  , at'StartingBid :: !Lovelace
  -- ^ Bids cannot be lower than this number.
  , at'MinBidIncrement :: !Lovelace
  -- ^ New bids can only supersede the standing bid if they exceed it
  -- by this increment.
  , at'MinDepositAmount :: !Lovelace
  -- ^ Minimal amount of ADA that the seller requests
  -- each bidder to place as a bidder deposit for the auction.
  -- This is only enforced off-chain at the seller's discretion.
  }
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Validation
-- -------------------------------------------------------------------------

validateAuctionTerms ::
  AuctionTerms ->
  Validation [AuctionTerms'Error] ()
validateAuctionTerms aTerms@AuctionTerms {..} =
  --
  -- (AT01) The seller pubkey hash corresponds to the seller verification key.
  -- Note: this check only becomes possible on-chain in Plutus V3.
  -- https://github.com/input-output-hk/plutus/pull/5431
  (at'SellerPkh == verificationKeyHash at'SellerVk)
    `err` AuctionTerms'Error'SellerVkPkhMismatch
    --
    -- (AT02) Bidding ends after it the bidding start time.
    <> (at'BiddingStart < at'BiddingEnd)
    `err` AuctionTerms'Error'BiddingStartNotBeforeBiddingEnd
    --
    -- (AT03) The purchase deadline occurs after bidding ends.
    <> (at'BiddingEnd < at'PurchaseDeadline)
    `err` AuctionTerms'Error'BiddingEndNotBeforePurchaseDeadline
    --
    -- (AT04) Cleanup happens after the purchase deadline,
    -- so that the seller can claim the winning bidder's deposit
    -- if the auction lot is not sold
    <> (at'PurchaseDeadline < at'Cleanup)
    `err` AuctionTerms'Error'PurchaseDeadlineNotBeforeCleanup
    --
    -- (AT05) New bids must be larger than the standing bid.
    <> (at'MinBidIncrement > Lovelace 0)
    `err` AuctionTerms'Error'NonPositiveMinBidIncrement
    --
    -- (AT06) The auction fee for each delegate must contain
    -- the min 2 ADA for the utxos that will be sent to the delegates
    -- during fee distribution.
    <> (at'StartingBid > totalAuctionFees aTerms)
    `err` AuctionTerms'Error'InvalidStartingBid
    --
    -- (AT07) The auction fees for all delegates must be covered by
    -- the starting bid.
    <> (at'AuctionFeePerDelegate > minAuctionFee)
    `err` AuctionTerms'Error'InvalidAuctionFeePerDelegate
    --
    -- (AT08) There must be at least one delegate.
    <> (length at'Delegates > 0)
    `err` AuctionTerms'Error'NoDelegates

minAuctionFee :: Lovelace
minAuctionFee = Lovelace 2_500_00

totalAuctionFees :: AuctionTerms -> Lovelace
totalAuctionFees AuctionTerms {..}
  | Lovelace x <- at'AuctionFeePerDelegate =
      Lovelace $
        x * fromIntegral (length at'Delegates)
