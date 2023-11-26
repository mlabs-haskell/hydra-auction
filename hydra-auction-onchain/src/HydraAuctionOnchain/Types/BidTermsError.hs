module HydraAuctionOnchain.Types.BidTermsError (
  BidTerms'Error (..),
) where

import HydraAuctionOnchain.Lib.Error (ToErrorCode (..))

data BidTerms'Error
  = BidTerms'Error'BidderInfo
  | BidTerms'Error'InvalidBidderSignature
  | BidTerms'Error'InvalidSellerSignature

instance ToErrorCode BidTerms'Error where
  toErrorCode = \case
    BidTerms'Error'BidderInfo ->
      "BT01"
    BidTerms'Error'InvalidBidderSignature ->
      "BT02"
    BidTerms'Error'InvalidSellerSignature ->
      "BT03"
