module HydraAuctionOnchain.Types.BidTerms (
  BidTerms (..),
  BidTerms'Error (..),
  validateBidTerms,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1 (CurrencySymbol, PubKeyHash)
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins

import HydraAuctionOnchain.Lib.Error (eCode, err)

import HydraAuctionOnchain.Types.AuctionTerms (AuctionTerms (..))
import HydraAuctionOnchain.Types.BidTermsError (BidTerms'Error (..))
import HydraAuctionOnchain.Types.BidderInfo (BidderInfo (..))

data BidTerms = BidTerms
  { bt'Bidder :: BidderInfo
  -- ^ Bidder that submitted the bid.
  , bt'BidPrice :: Integer
  -- ^ Price that the bidder bid to buy the auction lot.
  , bt'BidderSignature :: BuiltinByteString
  -- ^ Bidder's signature (via bi'BidderVk . bt'Bidder) of the
  -- (ai'AuctionId, bt'BidPrice, bi'BidderPkh) tuple,
  -- authorizing a bid at that price to be placed in the auction
  -- and bi'BidderPkh to buy the auction lot if the bid wins.
  , bt'SellerSignature :: BuiltinByteString
  -- ^ Seller's signature (via at'SellerVk) of the
  -- (ai'AuctionId, bi'BidderVk) tuple,
  -- authorizing the bidder bi'BidderVk to place bids in the auction.
  }

-- -------------------------------------------------------------------------
-- Validation
-- -------------------------------------------------------------------------

{-# INLINEABLE validateBidTerms #-}
validateBidTerms ::
  AuctionTerms ->
  CurrencySymbol ->
  BidTerms ->
  Bool
validateBidTerms AuctionTerms {..} auctionId BidTerms {..}
  | BidderInfo {..} <- bt'Bidder =
      --
      -- (BT01) The bidder's info is correct.
      -- This check is redundant on-chain until PlutusV3.
      -- validateBidderInfo bt'Bidder &&
      --
      -- (BT02) The seller authorized the bidder
      -- to participate in the auction.
      verifyEd25519Signature
        at'SellerVk
        (sellerSignatureMessage auctionId bi'BidderVk)
        bt'SellerSignature
        `err` $(eCode BidTerms'Error'InvalidSellerSignature)
        --
        -- (BT03) The bidder authorized the bid
        -- to be submitted in the auction.
        && verifyEd25519Signature
          bi'BidderVk
          (bidderSignatureMessage auctionId bt'BidPrice bi'BidderPkh)
          bt'BidderSignature
        `err` $(eCode BidTerms'Error'InvalidBidderSignature)

{-# INLINEABLE bidderSignatureMessage #-}
bidderSignatureMessage ::
  CurrencySymbol ->
  Integer ->
  PubKeyHash ->
  BuiltinByteString
bidderSignatureMessage auctionId bidPrice bidderPkh =
  serialise auctionId
    <> serialise bidderPkh
    <> serialise bidPrice

{-# INLINEABLE sellerSignatureMessage #-}
sellerSignatureMessage ::
  CurrencySymbol ->
  BuiltinByteString ->
  BuiltinByteString
sellerSignatureMessage auctionId bidderVk =
  serialise auctionId
    <> serialise bidderVk

{-# INLINEABLE serialise #-}
serialise :: PlutusTx.ToData a => a -> BuiltinByteString
serialise = Builtins.serialiseData . PlutusTx.toBuiltinData

-- -------------------------------------------------------------------------
-- Plutus instances
-- -------------------------------------------------------------------------
PlutusTx.unstableMakeIsData ''BidTerms

instance Eq BidTerms where
  (BidTerms x1 x2 x3 x4)
    == (BidTerms y1 y2 y3 y4) =
      x1 == y1
        && x2 == y2
        && x3 == y3
        && x4 == y4
