module HydraAuctionOffchain.Types.BidTerms (
  BidTerms (..),
  BidTerms'Error (..),
  validateBidTerms,
) where

import GHC.Generics (Generic)
import Prelude

import Data.Validation (Validation)

import Cardano.Api.Shelley (
  Lovelace (..),
  PolicyId (..),
  SerialiseAsRawBytes (..),
 )

import HydraAuctionOffchain.Lib.Crypto (
  Hash,
  PaymentKey,
  Signature,
  VerificationKey,
  serialiseLovelace,
  verifySignature,
 )
import HydraAuctionOffchain.Lib.Validation (err, errWith)

import Cardano.Crypto.Hash (ByteString)
import HydraAuctionOffchain.Types.AuctionTerms (AuctionTerms (..))
import HydraAuctionOffchain.Types.BidderInfo (
  BidderInfo (..),
  validateBidderInfo,
 )

import HydraAuctionOffchain.Types.BidTermsError (
  BidTerms'Error (..),
 )

data BidTerms = BidTerms
  { bt'Bidder :: BidderInfo
  -- ^ Bidder that submitted the bid.
  , bt'BidPrice :: Lovelace
  -- ^ Price that the bidder bid to buy the auction lot.
  , bt'BidderSignature :: Signature
  -- ^ Bidder's signature (via bi'BidderVk . bt'Bidder) of the
  -- (ai'AuctionId, bt'BidPrice, bi'BidderPkh) tuple,
  -- authorizing a bid at that price to be placed in the auction
  -- and bi'BidderPkh to buy the auction lot if the bid wins.
  , bt'SellerSignature :: Signature
  -- ^ Seller's signature (via at'SellerVk) of the
  -- (ai'AuctionId, bi'BidderVk) tuple,
  -- authorizing the bidder bi'BidderVk to place bids in the auction.
  }
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Validation
-- -------------------------------------------------------------------------

validateBidTerms ::
  AuctionTerms ->
  PolicyId ->
  BidTerms ->
  Validation [BidTerms'Error] ()
validateBidTerms AuctionTerms {..} auctionId BidTerms {..}
  | BidderInfo {..} <- bt'Bidder =
      --
      -- (BT01) The bidder's info is correct.
      validateBidderInfo bt'Bidder
        `errWith` BidTerms'Error'BidderInfo
        --
        -- (BT02) The seller authorized the bidder
        -- to participate in the auction.
        <> verifySignature
          at'SellerVk
          (sellerSignatureMessage auctionId bi'BidderVk)
          bt'SellerSignature
        `err` BidTerms'Error'InvalidSellerSignature
        --
        -- (BT03) The bidder authorized the bid
        -- to be submitted in the auction.
        <> verifySignature
          bi'BidderVk
          (bidderSignatureMessage auctionId bt'BidPrice bi'BidderPkh)
          bt'BidderSignature
        `err` BidTerms'Error'InvalidBidderSignature

bidderSignatureMessage ::
  PolicyId ->
  Lovelace ->
  Hash PaymentKey ->
  ByteString
bidderSignatureMessage auctionId bidPrice bidderPkh =
  serialiseToRawBytes auctionId
    <> serialiseToRawBytes bidderPkh
    <> serialiseLovelace bidPrice

sellerSignatureMessage ::
  PolicyId ->
  VerificationKey PaymentKey ->
  ByteString
sellerSignatureMessage auctionId bidderVk =
  serialiseToRawBytes auctionId
    <> serialiseToRawBytes bidderVk
