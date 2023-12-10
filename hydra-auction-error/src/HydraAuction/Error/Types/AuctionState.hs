module HydraAuction.Error.Types.AuctionState (
  AuctionEscrowState'Error (..),
  NewBid'Error (..),
) where

import Prelude

import Data.Universe (Universe (..), universeGeneric)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))
import HydraAuction.Error.Types.BidTerms (BidTerms'Error (..))

data AuctionEscrowState'Error
  = -- Transition to StartBidding
    AuctionEscrowState'SB'Error'InvalidOldState
  | AuctionEscrowState'SB'Error'InvalidNewState
  | -- Transition to AuctionConcluded
    AuctionEscrowState'AC'Error'InvalidOldState
  | AuctionEscrowState'AC'Error'InvalidNewState
  deriving stock (Bounded, Enum, Eq, Generic, Show)

data NewBid'Error
  = NewBid'Error'EmptyNewBid
  | NewBid'Error'InvalidNewBidTerms ![BidTerms'Error]
  | NewBid'Error'InvalidStartingBid
  | NewBid'Error'InvalidBidIncrement
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe AuctionEscrowState'Error

instance Universe NewBid'Error where
  universe = universeGeneric

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix AuctionEscrowState'Error where
  errorCodePrefix = const "AUST"

instance ErrorCodePrefix NewBid'Error where
  errorCodePrefix = const "NEWB"
