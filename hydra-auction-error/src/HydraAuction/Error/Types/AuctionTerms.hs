module HydraAuction.Error.Types.AuctionTerms (
  AuctionTerms'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))

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

instance ToErrorCode AuctionTerms'Error where
  toErrorCode = \case
    AuctionTerms'Error'SellerVkPkhMismatch ->
      "AuctionTerms01"
    AuctionTerms'Error'BiddingStartNotBeforeBiddingEnd ->
      "AuctionTerms02"
    AuctionTerms'Error'BiddingEndNotBeforePurchaseDeadline ->
      "AuctionTerms03"
    AuctionTerms'Error'PurchaseDeadlineNotBeforeCleanup ->
      "AuctionTerms04"
    AuctionTerms'Error'NonPositiveMinBidIncrement ->
      "AuctionTerms05"
    AuctionTerms'Error'InvalidStartingBid ->
      "AuctionTerms06"
    AuctionTerms'Error'InvalidAuctionFeePerDelegate ->
      "AuctionTerms07"
    AuctionTerms'Error'NoDelegates ->
      "AuctionTerms08"
