module HydraAuction.Error.Types.BidTerms (
  BidTerms'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))
import HydraAuction.Error.Types.BidderInfo (BidderInfo'Error (..))

data BidTerms'Error
  = BidTerms'Error'BidderInfo ![BidderInfo'Error]
  | BidTerms'Error'InvalidBidderSignature
  | BidTerms'Error'InvalidSellerSignature
  deriving stock (Eq, Generic, Show)

instance ToErrorCode BidTerms'Error where
  toErrorCode = \case
    BidTerms'Error'BidderInfo _ ->
      "BT01"
    BidTerms'Error'InvalidBidderSignature ->
      "BT02"
    BidTerms'Error'InvalidSellerSignature ->
      "BT03"
