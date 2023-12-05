module HydraAuction.Offchain.Types.BidTerms (
  BidTerms (..),
  BidTerms'Error (..),
  validateBidTerms,
  fromPlutusBidTerms,
  toPlutusBidTerms,
) where

import GHC.Generics (Generic)
import Prelude

import Data.Function ((&))
import Data.Validation (Validation)

import Cardano.Api.Shelley (
  Lovelace (..),
  PolicyId (..),
  SerialiseAsRawBytes (..),
 )
import Cardano.Crypto.Hash (ByteString)

import HydraAuction.Error.Types.BidTerms (
  BidTerms'Error (..),
 )
import HydraAuction.Offchain.Lib.Codec.Onchain (
  fromPlutusLovelace,
  fromPlutusSignature,
  toPlutusLovelace,
  toPlutusSignature,
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
validateBidTerms AuctionTerms {..} auctionId BidTerms {..}
  | BidderInfo {..} <- bt'Bidder =
      --
      -- (BidTerms01)
      -- The bidder's info is correct.
      validateBidderInfo bt'Bidder
        `errWith` BidTerms'Error'BidderInfo
        --
        -- (BidTerms02)
        -- The seller authorized the bidder
        -- to participate in the auction.
        <> verifySignature
          at'SellerVk
          (sellerSignatureMessage auctionId bi'BidderVk)
          bt'SellerSignature
        `err` BidTerms'Error'InvalidSellerSignature
        --
        -- (BidTerms03)
        -- The bidder authorized the bid
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
