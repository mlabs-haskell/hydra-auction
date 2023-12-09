module HydraAuction.Error.Types.BidderInfo (
  BidderInfo'Error (..),
) where

import Prelude

import Data.Universe (Universe)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))

data BidderInfo'Error
  = BidderInfo'Error'BidderVkPkhMismatch
  deriving stock (Bounded, Enum, Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe BidderInfo'Error

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix BidderInfo'Error where
  errorCodePrefix = const "BIIN"
