module HydraAuctionOffchain.Types.AuctionTermsError (
  AuctionTerms'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

data AuctionTerms'Error
  = AuctionTerms'Error'SellerVkPkhMismatch
  | AuctionTerms'Error'BiddingStartNotBeforeBiddingEnd
  | AuctionTerms'Error'BiddingEndNotBeforePurchaseDeadline
  | AuctionTerms'Error'PurchaseDeadlineNotBeforeCleanup
  | AuctionTerms'Error'NonPositiveMinBidIncrement
  | AuctionTerms'Error'InvalidStartingBid
  | AuctionTerms'Error'InvalidAuctionFeePerDelegate
  | AuctionTerms'Error'NoDelegates
  deriving stock (Eq, Generic, Show)
