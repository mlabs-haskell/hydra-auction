module HydraAuction.Error.Types.BidTerms (
  BidTerms'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))
import HydraAuction.Error.Types.BidderInfo (BidderInfo'Error (..))

data BidTerms'Error
  = BidTerms'Error'BidderInfo ![BidderInfo'Error]
  | BidTerms'Error'InvalidSellerSignature
  | BidTerms'Error'InvalidBidderSignature
  deriving stock (Eq, Generic, Show)

instance ToErrorCode BidTerms'Error where
  toErrorCode = \case
    BidTerms'Error'BidderInfo _ ->
      "BidTerms01"
    BidTerms'Error'InvalidSellerSignature ->
      "BidTerms02"
    BidTerms'Error'InvalidBidderSignature ->
      "BidTerms03"
