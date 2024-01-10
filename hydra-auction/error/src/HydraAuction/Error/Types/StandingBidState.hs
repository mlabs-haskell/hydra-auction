module HydraAuction.Error.Types.StandingBidState (
  NewBid'Error (..),
) where

import Prelude

import Data.Universe (Universe (..), universeGeneric)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))
import HydraAuction.Error.Types.BidTerms (BidTerms'Error (..))

data NewBid'Error
  = NewBid'Error'EmptyNewBid
  | NewBid'Error'InvalidNewBidTerms ![BidTerms'Error]
  | NewBid'Error'InvalidStartingBid
  | NewBid'Error'InvalidBidIncrement
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe NewBid'Error where
  universe = universeGeneric

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix NewBid'Error where
  errorCodePrefix = const "NEWB"
