module HydraAuctionOnchain.Types.AuctionStateError (
  Buyer'Error (..),
  NewBid'Error (..),
) where

import HydraAuctionOnchain.Lib.Error (ToErrorCode (..))

data Buyer'Error
  = Buyer'Error'EmptyStandingBid
  | Buyer'Error'BuyerVkPkhMismatch
  | Buyer'Error'InvalidBidTerms

instance ToErrorCode Buyer'Error where
  toErrorCode = \case
    Buyer'Error'EmptyStandingBid ->
      "BU01"
    Buyer'Error'BuyerVkPkhMismatch ->
      "BU02"
    Buyer'Error'InvalidBidTerms ->
      "BU03"

data NewBid'Error
  = NewBid'Error'EmptyNewBid
  | NewBid'Error'InvalidNewBidTerms
  | NewBid'Error'InvalidStartingBid
  | NewBid'Error'InvalidBidIncrement

instance ToErrorCode NewBid'Error where
  toErrorCode = \case
    NewBid'Error'EmptyNewBid ->
      "NB01"
    NewBid'Error'InvalidNewBidTerms ->
      "NB02"
    NewBid'Error'InvalidStartingBid ->
      "NB03"
    NewBid'Error'InvalidBidIncrement ->
      "NB04"
