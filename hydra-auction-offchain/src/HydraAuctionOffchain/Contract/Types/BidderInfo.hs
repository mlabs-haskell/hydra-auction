module HydraAuctionOffchain.Contract.Types.BidderInfo (
  BidderInfo (..),
  BidderInfo'Error (..),
  validateBidderInfo,
) where

import Prelude

import Data.Foldable (fold)
import Data.Validation (Validation)
import GHC.Generics (Generic)

import HydraAuctionOffchain.Lib.Crypto (
  Hash,
  Key (verificationKeyHash),
  PaymentKey,
  VerificationKey,
 )
import HydraAuctionOffchain.Lib.Validation (err)

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

data BidderInfo'Error
  = BidderInfo'Error'BidderVkPkhMismatch
  deriving stock (Eq, Generic, Show)

validateBidderInfo ::
  BidderInfo ->
  Validation [BidderInfo'Error] ()
validateBidderInfo BidderInfo {..} =
  fold
    [ (bi'BidderPkh == verificationKeyHash bi'BidderVk)
        `err` BidderInfo'Error'BidderVkPkhMismatch
    ]
