module HydraAuction.Error.Types.AuctionEscrowState (
  AuctionEscrowState'Error (..),
) where

import Prelude

import Data.Universe (Universe (..))
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))

data AuctionEscrowState'Error
  = -- Transition to StartBidding
    AuctionEscrowState'SB'Error'InvalidOldState
  | AuctionEscrowState'SB'Error'InvalidNewState
  | -- Transition to AuctionConcluded
    AuctionEscrowState'AC'Error'InvalidOldState
  | AuctionEscrowState'AC'Error'InvalidNewState
  deriving stock (Bounded, Enum, Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe AuctionEscrowState'Error

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix AuctionEscrowState'Error where
  errorCodePrefix = const "AUST"
