module HydraAuction.Error.Types.AuctionState (
  Buyer'Error (..),
  NewBid'Error (..),
) where

import Prelude

import Data.Universe (Universe (..), universeGeneric)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))
import HydraAuction.Error.Types.BidTerms (BidTerms'Error (..))

data Buyer'Error
  = Buyer'Error'EmptyStandingBid
  | Buyer'Error'BuyerVkPkhMismatch
  | Buyer'Error'InvalidBidTerms ![BidTerms'Error]
  deriving stock (Eq, Generic, Show)

data NewBid'Error
  = NewBid'Error'EmptyNewBid
  | NewBid'Error'InvalidNewBidTerms ![BidTerms'Error]
  | NewBid'Error'InvalidStartingBid
  | NewBid'Error'InvalidBidIncrement
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe Buyer'Error where
  universe = universeGeneric

instance Universe NewBid'Error where
  universe = universeGeneric

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix Buyer'Error where
  errorCodePrefix = const "BUYR"

instance ErrorCodePrefix NewBid'Error where
  errorCodePrefix = const "NEWB"
