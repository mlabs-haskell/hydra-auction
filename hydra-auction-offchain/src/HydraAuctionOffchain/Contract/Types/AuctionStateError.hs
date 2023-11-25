module HydraAuctionOffchain.Contract.Types.AuctionStateError (
  Buyer'Error (..),
  NewBid'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuctionOffchain.Contract.Types.BidTerms (
  BidTerms'Error,
 )

data Buyer'Error
  = Buyer'Error'EmptyStandingBid
  | Buyer'Error'BuyerVkPkhMismatch
  | Buyer'Error'InvalidBidTerms BidTerms'Error
  deriving stock (Eq, Generic, Show)

data NewBid'Error
  = NewBid'Error'EmptyNewBid
  | NewBid'Error'InvalidNewBidTerms BidTerms'Error
  | NewBid'Error'InvalidStartingBid
  | NewBid'Error'InvalidBidIncrement
  deriving stock (Eq, Generic, Show)
