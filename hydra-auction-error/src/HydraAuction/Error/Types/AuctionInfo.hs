module HydraAuction.Error.Types.AuctionInfo (
  AuctionInfo'Error (..),
) where

import Prelude

import Data.Universe (Universe (..), universeGeneric)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))
import HydraAuction.Error.Types.AuctionTerms (AuctionTerms'Error)

newtype AuctionInfo'Error
  = AuctionInfo'Error'InvalidAuctionTerms [AuctionTerms'Error]
  -- AuctionInfo'Error'InvalidAuctionIdCurrencySymbol
  -- AuctionInfo'Error'InvalidScriptAddresses
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe AuctionInfo'Error where
  universe = universeGeneric

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix AuctionInfo'Error where
  errorCodePrefix = const "AUIN"
