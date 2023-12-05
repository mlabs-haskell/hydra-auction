module HydraAuction.Error.Types.AuctionState (
  Buyer'Error (..),
  NewBid'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))
import HydraAuction.Error.Types.BidTerms (BidTerms'Error (..))

data Buyer'Error
  = Buyer'Error'EmptyStandingBid
  | Buyer'Error'BuyerVkPkhMismatch
  | Buyer'Error'InvalidBidTerms ![BidTerms'Error]
  deriving stock (Eq, Generic, Show)

instance ToErrorCode Buyer'Error where
  toErrorCode = \case
    Buyer'Error'EmptyStandingBid ->
      "Buyer01"
    Buyer'Error'BuyerVkPkhMismatch ->
      "Buyer02"
    Buyer'Error'InvalidBidTerms _ ->
      "Buyer03"

data NewBid'Error
  = NewBid'Error'EmptyNewBid
  | NewBid'Error'InvalidNewBidTerms ![BidTerms'Error]
  | NewBid'Error'InvalidStartingBid
  | NewBid'Error'InvalidBidIncrement
  deriving stock (Eq, Generic, Show)

instance ToErrorCode NewBid'Error where
  toErrorCode = \case
    NewBid'Error'EmptyNewBid ->
      "NewBid01"
    NewBid'Error'InvalidNewBidTerms _ ->
      "NewBid02"
    NewBid'Error'InvalidStartingBid ->
      "NewBid03"
    NewBid'Error'InvalidBidIncrement ->
      "NewBid04"
