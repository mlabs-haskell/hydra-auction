module HydraAuction.Offchain.Types.AuctionTerms (
  AuctionTerms (..),
  AuctionTerms'Error (..),
  totalAuctionFees,
  validateAuctionTerms,
  toPlutusAuctionTerms,
  fromPlutusAuctionTerms,
) where

import GHC.Generics (Generic)
import Prelude

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Traversable (for)
import Data.Validation (Validation)

import Cardano.Api.Shelley (
  AssetId (..),
  Lovelace (..),
 )

import HydraAuction.Error.Types.AuctionTerms (
  AuctionTerms'Error (..),
 )
import HydraAuction.Offchain.Lib.Codec.Onchain (
  fromPlutusAssetId,
  fromPlutusLovelace,
  fromPlutusUTCTimeMilli,
  fromPlutusVKey,
  fromPlutusVKeyHash,
  toPlutusAssetId,
  toPlutusLovelace,
  toPlutusUTCTimeMilli,
  toPlutusVKey,
  toPlutusVKeyHash,
 )
import HydraAuction.Offchain.Lib.Crypto (
  Hash,
  Key (verificationKeyHash),
  PaymentKey,
  VerificationKey,
 )
import HydraAuction.Offchain.Lib.Time (UTCTimeMilli)
import HydraAuction.Offchain.Lib.Validation (err)

import HydraAuction.Onchain.Types.AuctionTerms qualified as O

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
  , at'BiddingStart :: !UTCTimeMilli
  -- ^ Start time of the bidding period.
  , at'BiddingEnd :: !UTCTimeMilli
  -- ^ End time of the bidding period.
  , at'PurchaseDeadline :: !UTCTimeMilli
  -- ^ Time by which the winning bidder can buy the auction lot.
  -- At and after this time, the winning bidder forfeits its bidder deposit
  -- if the auction lot has not been purchased.
  , at'Cleanup :: !UTCTimeMilli
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
  -- (AuctionTerms01)
  -- The seller pubkey hash corresponds to the seller verification key.
  -- Note: this check only becomes possible on-chain in Plutus V3.
  -- https://github.com/input-output-hk/plutus/pull/5431
  (at'SellerPkh == verificationKeyHash at'SellerVk)
    `err` AuctionTerms'Error'SellerVkPkhMismatch
    --
    -- (AuctionTerms02)
    -- Bidding ends after it the bidding start time.
    <> (at'BiddingStart < at'BiddingEnd)
    `err` AuctionTerms'Error'BiddingStartNotBeforeBiddingEnd
    --
    -- (AuctionTerms03)
    -- The purchase deadline occurs after bidding ends.
    <> (at'BiddingEnd < at'PurchaseDeadline)
    `err` AuctionTerms'Error'BiddingEndNotBeforePurchaseDeadline
    --
    -- (AuctionTerms04)
    -- Cleanup happens after the purchase deadline,
    -- so that the seller can claim the winning bidder's deposit
    -- if the auction lot is not sold
    <> (at'PurchaseDeadline < at'Cleanup)
    `err` AuctionTerms'Error'PurchaseDeadlineNotBeforeCleanup
    --
    -- (AuctionTerms05)
    -- New bids must be larger than the standing bid.
    <> (at'MinBidIncrement > Lovelace 0)
    `err` AuctionTerms'Error'NonPositiveMinBidIncrement
    --
    -- (AuctionTerms06)
    -- The auction fee for each delegate must contain
    -- the min 2 ADA for the utxos that will be sent to the delegates
    -- during fee distribution.
    <> (at'StartingBid > totalAuctionFees aTerms)
    `err` AuctionTerms'Error'InvalidStartingBid
    --
    -- (AuctionTerms07)
    -- The auction fees for all delegates must be covered by
    -- the starting bid.
    <> (at'AuctionFeePerDelegate > minAuctionFee)
    `err` AuctionTerms'Error'InvalidAuctionFeePerDelegate
    --
    -- (AuctionTerms08)
    -- There must be at least one delegate.
    <> (length at'Delegates > 0)
    `err` AuctionTerms'Error'NoDelegates

minAuctionFee :: Lovelace
minAuctionFee = Lovelace 2_500_00

totalAuctionFees :: AuctionTerms -> Lovelace
totalAuctionFees AuctionTerms {..}
  | Lovelace x <- at'AuctionFeePerDelegate =
      Lovelace $
        x * fromIntegral (length at'Delegates)

-- -------------------------------------------------------------------------
-- Conversion to onchain
-- -------------------------------------------------------------------------
toPlutusAuctionTerms :: AuctionTerms -> O.AuctionTerms
toPlutusAuctionTerms AuctionTerms {..} =
  O.AuctionTerms
    { O.at'AuctionLot =
        at'AuctionLot & toPlutusAssetId
    , --
      O.at'SellerPkh =
        at'SellerPkh & toPlutusVKeyHash
    , --
      O.at'SellerVk =
        at'SellerVk & toPlutusVKey
    , --
      O.at'Delegates =
        at'Delegates <&> toPlutusVKeyHash
    , --
      O.at'BiddingStart =
        at'BiddingStart & toPlutusUTCTimeMilli
    , --
      O.at'BiddingEnd =
        at'BiddingEnd & toPlutusUTCTimeMilli
    , --
      O.at'PurchaseDeadline =
        at'PurchaseDeadline & toPlutusUTCTimeMilli
    , --
      O.at'Cleanup =
        at'Cleanup & toPlutusUTCTimeMilli
    , --
      O.at'AuctionFeePerDelegate =
        at'AuctionFeePerDelegate & toPlutusLovelace
    , --
      O.at'StartingBid =
        at'StartingBid & toPlutusLovelace
    , --
      O.at'MinBidIncrement =
        at'MinBidIncrement & toPlutusLovelace
    , --
      O.at'MinDepositAmount =
        at'MinDepositAmount & toPlutusLovelace
    }

-- -------------------------------------------------------------------------
-- Conversion from onchain
-- -------------------------------------------------------------------------
fromPlutusAuctionTerms :: O.AuctionTerms -> Maybe AuctionTerms
fromPlutusAuctionTerms O.AuctionTerms {..} = do
  m'at'AuctionLot <-
    at'AuctionLot & fromPlutusAssetId
  --
  m'at'SellerPkh <-
    at'SellerPkh & fromPlutusVKeyHash
  --
  m'at'SellerVk <-
    at'SellerVk & fromPlutusVKey
  --
  m'at'Delegates <-
    at'Delegates `for` fromPlutusVKeyHash
  --
  let m'at'BiddingStart =
        at'BiddingStart & fromPlutusUTCTimeMilli
  --
  let m'at'BiddingEnd =
        at'BiddingEnd & fromPlutusUTCTimeMilli
  --
  let m'at'PurchaseDeadline =
        at'PurchaseDeadline & fromPlutusUTCTimeMilli
  --
  let m'at'Cleanup =
        at'Cleanup & fromPlutusUTCTimeMilli
  --
  let m'at'AuctionFeePerDelegate =
        at'AuctionFeePerDelegate & fromPlutusLovelace
  --
  let m'at'StartingBid =
        at'StartingBid & fromPlutusLovelace
  --
  let m'at'MinBidIncrement =
        at'MinBidIncrement & fromPlutusLovelace
  --
  let m'at'MinDepositAmount =
        at'MinDepositAmount & fromPlutusLovelace
  --
  pure $
    AuctionTerms
      { at'AuctionLot = m'at'AuctionLot
      , at'SellerPkh = m'at'SellerPkh
      , at'SellerVk = m'at'SellerVk
      , at'Delegates = m'at'Delegates
      , at'BiddingStart = m'at'BiddingStart
      , at'BiddingEnd = m'at'BiddingEnd
      , at'PurchaseDeadline = m'at'PurchaseDeadline
      , at'Cleanup = m'at'Cleanup
      , at'AuctionFeePerDelegate = m'at'AuctionFeePerDelegate
      , at'StartingBid = m'at'StartingBid
      , at'MinBidIncrement = m'at'MinBidIncrement
      , at'MinDepositAmount = m'at'MinDepositAmount
      }
