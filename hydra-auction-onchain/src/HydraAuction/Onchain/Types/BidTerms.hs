module HydraAuction.Onchain.Types.BidTerms (
  BidTerms (..),
  BidTerms'Error (..),
  validateBidTerms,
  sellerPayout,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V2 (
  CurrencySymbol,
  PubKeyHash,
 )
import PlutusTx qualified

import HydraAuction.Error.Types.BidTerms (BidTerms'Error (..))
import HydraAuction.Onchain.Lib.Error (eCode, err)
import HydraAuction.Onchain.Lib.Serialise (serialise)
import HydraAuction.Onchain.Types.AuctionTerms (
  AuctionTerms (..),
  totalAuctionFees,
 )
import HydraAuction.Onchain.Types.BidderInfo (BidderInfo (..))

data BidTerms = BidTerms
  { bt'Bidder :: !BidderInfo
  -- ^ Bidder that submitted the bid.
  , bt'BidPrice :: !Integer
  -- ^ Price that the bidder bid to buy the auction lot.
  , bt'BidderSignature :: !BuiltinByteString
  -- ^ Bidder's signature (via bi'BidderVk . bt'Bidder) of the
  -- (ai'AuctionId, bt'BidPrice, bi'BidderPkh) tuple,
  -- authorizing a bid at that price to be placed in the auction
  -- and bi'BidderPkh to buy the auction lot if the bid wins.
  , bt'SellerSignature :: !BuiltinByteString
  -- ^ Seller's signature (via at'SellerVk) of the
  -- (ai'AuctionId, bi'BidderVk) tuple,
  -- authorizing the bidder bi'BidderVk to place bids in the auction.
  }

instance Eq BidTerms where
  (BidTerms x1 x2 x3 x4)
    == (BidTerms y1 y2 y3 y4) =
      x1 == y1
        && x2 == y2
        && x3 == y3
        && x4 == y4

PlutusTx.unstableMakeIsData ''BidTerms

-- -------------------------------------------------------------------------
-- Seller payout
-- -------------------------------------------------------------------------

sellerPayout :: AuctionTerms -> BidTerms -> Integer
sellerPayout auTerms BidTerms {..} =
  bt'BidPrice - totalAuctionFees auTerms
--
{-# INLINEABLE sellerPayout #-}

-- -------------------------------------------------------------------------
-- Validation
-- -------------------------------------------------------------------------

validateBidTerms ::
  AuctionTerms ->
  CurrencySymbol ->
  BidTerms ->
  Bool
validateBidTerms AuctionTerms {..} auctionId BidTerms {..}
  | BidderInfo {..} <- bt'Bidder =
      --
      -- The bidder's info is correct.
      -- This check is not possible on-chain until PlutusV3.
      -- validateBidderInfo bt'Bidder &&
      --
      -- The seller authorized the bidder
      -- to participate in the auction.
      verifyEd25519Signature
        at'SellerVk
        (sellerSignatureMessage auctionId bi'BidderVk)
        bt'SellerSignature
        `err` $(eCode BidTerms'Error'InvalidSellerSignature)
        --
        -- The bidder authorized the bid
        -- to be submitted in the auction.
        && verifyEd25519Signature
          bi'BidderVk
          (bidderSignatureMessage auctionId bt'BidPrice bi'BidderPkh)
          bt'BidderSignature
        `err` $(eCode BidTerms'Error'InvalidBidderSignature)
--
{-# INLINEABLE validateBidTerms #-}

bidderSignatureMessage ::
  CurrencySymbol ->
  Integer ->
  PubKeyHash ->
  BuiltinByteString
bidderSignatureMessage auctionId bidPrice bidderPkh =
  serialise auctionId
    <> serialise bidderPkh
    <> serialise bidPrice
--
{-# INLINEABLE bidderSignatureMessage #-}

sellerSignatureMessage ::
  CurrencySymbol ->
  BuiltinByteString ->
  BuiltinByteString
sellerSignatureMessage auctionId bidderVk =
  serialise auctionId
    <> serialise bidderVk
--
{-# INLINEABLE sellerSignatureMessage #-}
