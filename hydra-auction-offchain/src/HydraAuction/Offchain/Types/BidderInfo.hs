module HydraAuction.Offchain.Types.BidderInfo (
  BidderInfo (..),
  BidderInfo'Error (..),
  validateBidderInfo,
) where

import GHC.Generics (Generic)
import Prelude

import Data.Validation (Validation)

import HydraAuction.Error.Types.BidderInfo (
  BidderInfo'Error (..),
 )
import HydraAuction.Offchain.Lib.Crypto (
  Hash,
  Key (verificationKeyHash),
  PaymentKey,
  VerificationKey,
 )
import HydraAuction.Offchain.Lib.Validation (err)

data BidderInfo = BidderInfo
  { bi'BidderPkh :: Hash PaymentKey
  -- ^ Bidder's pubkey hash, which can spend this bidder deposit
  -- to buy the auction lot if a bid placed by bi'BidderVk wins
  -- or reclaim this bid deposit if someone else's bid wins.
  , bi'BidderVk :: VerificationKey PaymentKey
  -- ^ Bidder's verification, which can authorize bids that allow
  -- the seller at'SellerPkh to claim this bidder deposit
  -- if the bid placed by bi'BidderVk won but the auction lot
  -- wasn't purchased by the deadline.
  }
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Validation
-- -------------------------------------------------------------------------

validateBidderInfo ::
  BidderInfo ->
  Validation [BidderInfo'Error] ()
validateBidderInfo BidderInfo {..} =
  --
  -- (BI01) The bidder's hashed payment verification key corresponds
  -- to the bidder's payment verification key.
  -- Note: this check only becomes possible on-chain in Plutus V3.
  -- https://github.com/input-output-hk/plutus/pull/5431
  (bi'BidderPkh == verificationKeyHash bi'BidderVk)
    `err` BidderInfo'Error'BidderVkPkhMismatch
