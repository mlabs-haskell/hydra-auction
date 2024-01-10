module HydraAuction.Error.Types.AuctionTerms (
  AuctionTerms'Error (..),
) where

import Prelude

import Data.Universe (Universe)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))

data AuctionTerms'Error
  = AuctionTerms'Error'SellerVkPkhMismatch
  | AuctionTerms'Error'BiddingStartNotBeforeBiddingEnd
  | AuctionTerms'Error'BiddingEndNotBeforePurchaseDeadline
  | AuctionTerms'Error'PurchaseDeadlineNotBeforeCleanup
  | AuctionTerms'Error'NonPositiveMinBidIncrement
  | AuctionTerms'Error'InvalidStartingBid
  | AuctionTerms'Error'InvalidAuctionFeePerDelegate
  | AuctionTerms'Error'NoDelegates
  deriving stock (Bounded, Enum, Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe AuctionTerms'Error

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix AuctionTerms'Error where
  errorCodePrefix = const "AUTE"
