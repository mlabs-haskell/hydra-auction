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
      "AT01"
    AuctionTerms'Error'BiddingStartNotBeforeBiddingEnd ->
      "AT02"
    AuctionTerms'Error'BiddingEndNotBeforePurchaseDeadline ->
      "AT03"
    AuctionTerms'Error'PurchaseDeadlineNotBeforeCleanup ->
      "AT04"
    AuctionTerms'Error'NonPositiveMinBidIncrement ->
      "AT05"
    AuctionTerms'Error'InvalidStartingBid ->
      "AT06"
    AuctionTerms'Error'InvalidAuctionFeePerDelegate ->
      "AT07"
    AuctionTerms'Error'NoDelegates ->
      "AT08"
