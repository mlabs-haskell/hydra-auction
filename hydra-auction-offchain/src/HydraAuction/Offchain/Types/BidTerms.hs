module HydraAuction.Offchain.Types.BidTerms (
  BidTerms (..),
  BidTerms'Error (..),
  validateBidTerms,
  fromPlutusBidTerms,
  toPlutusBidTerms,
) where

import GHC.Generics (Generic)
import Prelude

import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Validation (Validation)

import Cardano.Api.Shelley (
  Lovelace (..),
  PolicyId (..),
  SerialiseAsRawBytes (..),
 )

import HydraAuction.Error.Types.BidTerms (
  BidTerms'Error (..),
 )
import HydraAuction.Offchain.Lib.Crypto (
  Hash,
  PaymentKey,
  Signature,
  VerificationKey,
  serialiseLovelace,
  verifySignature,
 )
import HydraAuction.Offchain.Lib.Validation (err, errWith)
import HydraAuction.Offchain.Types.AuctionTerms (AuctionTerms (..))
import HydraAuction.Offchain.Types.BidderInfo (
  BidderInfo (..),
  fromPlutusBidderInfo,
  toPlutusBidderInfo,
  validateBidderInfo,
 )
import Plutus.Cardano.Api.Codec (
  fromPlutusLovelace,
  fromPlutusSignature,
  toPlutusLovelace,
  toPlutusSignature,
 )

import HydraAuction.Onchain.Types.BidTerms qualified as O

data BidTerms = BidTerms
  { bt'Bidder :: !BidderInfo
  -- ^ Bidder that submitted the bid.
  , bt'BidPrice :: !Lovelace
  -- ^ Price that the bidder bid to buy the auction lot.
  , bt'BidderSignature :: !Signature
  -- ^ Bidder's signature (via bi'BidderVk . bt'Bidder) of the
  -- (ai'AuctionId, bt'BidPrice, bi'BidderPkh) tuple,
  -- authorizing a bid at that price to be placed in the auction
  -- and bi'BidderPkh to buy the auction lot if the bid wins.
  , bt'SellerSignature :: !Signature
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
validateBidTerms AuctionTerms {..} auctionCs BidTerms {..}
  | BidderInfo {..} <- bt'Bidder =
      --
      -- The bidder's info is correct.
      validateBidderInfo bt'Bidder
        `errWith` BidTerms'Error'BidderInfo
        --
        -- The seller authorized the bidder
        -- to participate in the auction.
        <> verifySignature
          at'SellerVk
          (sellerSignatureMessage auctionCs bi'BidderVk)
          bt'SellerSignature
        `err` BidTerms'Error'InvalidSellerSignature
        --
        -- The bidder authorized the bid
        -- to be submitted in the auction.
        <> verifySignature
          bi'BidderVk
          (bidderSignatureMessage auctionCs bi'BidderPkh bt'BidPrice)
          bt'BidderSignature
        `err` BidTerms'Error'InvalidBidderSignature

sellerSignatureMessage ::
  PolicyId ->
  VerificationKey PaymentKey ->
  ByteString
sellerSignatureMessage auctionCs bidderVk =
  serialiseToRawBytes auctionCs
    <> serialiseToRawBytes bidderVk

bidderSignatureMessage ::
  PolicyId ->
  Hash PaymentKey ->
  Lovelace ->
  ByteString
bidderSignatureMessage auctionCs bidderPkh bidPrice =
  serialiseToRawBytes auctionCs
    <> serialiseToRawBytes bidderPkh
    <> serialiseLovelace bidPrice

-- -------------------------------------------------------------------------
-- Conversion to onchain
-- -------------------------------------------------------------------------
toPlutusBidTerms :: BidTerms -> O.BidTerms
toPlutusBidTerms BidTerms {..} =
  O.BidTerms
    { O.bt'Bidder =
        bt'Bidder & toPlutusBidderInfo
    , --
      O.bt'BidPrice =
        bt'BidPrice & toPlutusLovelace
    , --
      O.bt'BidderSignature =
        bt'BidderSignature & toPlutusSignature
    , --
      O.bt'SellerSignature =
        bt'SellerSignature & toPlutusSignature
    }

-- -------------------------------------------------------------------------
-- Conversion from onchain
-- -------------------------------------------------------------------------
fromPlutusBidTerms :: O.BidTerms -> Maybe BidTerms
fromPlutusBidTerms O.BidTerms {..} = do
  m'bt'Bidder <-
    bt'Bidder & fromPlutusBidderInfo
  --
  let m'bt'BidPrice =
        bt'BidPrice & fromPlutusLovelace
  --
  m'bt'BidderSignature <-
    bt'BidderSignature & fromPlutusSignature
  --
  m'bt'SellerSignature <-
    bt'SellerSignature & fromPlutusSignature
  --
  pure $
    BidTerms
      { bt'Bidder = m'bt'Bidder
      , bt'BidPrice = m'bt'BidPrice
      , bt'BidderSignature = m'bt'BidderSignature
      , bt'SellerSignature = m'bt'SellerSignature
      }
