module HydraAuctionOffchain.Contract.Types.BidderInfoError (
  BidderInfo'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

data BidderInfo'Error
  = BidderInfo'Error'BidderVkPkhMismatch
  deriving stock (Eq, Generic, Show)
