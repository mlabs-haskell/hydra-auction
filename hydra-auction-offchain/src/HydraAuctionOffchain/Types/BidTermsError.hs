module HydraAuctionOffchain.Types.BidTermsError (
  BidTerms'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuctionOffchain.Types.BidderInfo (
  BidderInfo'Error,
 )

data BidTerms'Error
  = BidTerms'Error'BidderInfo BidderInfo'Error
  | BidTerms'Error'InvalidBidderSignature
  | BidTerms'Error'InvalidSellerSignature
  deriving stock (Eq, Generic, Show)
