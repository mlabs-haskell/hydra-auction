module HydraAuction.Error.Types.BidTerms (
  BidTerms'Error (..),
) where

import Prelude

import Data.Universe (Universe (..), universeGeneric)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))
import HydraAuction.Error.Types.BidderInfo (BidderInfo'Error (..))

data BidTerms'Error
  = BidTerms'Error'BidderInfo ![BidderInfo'Error]
  | BidTerms'Error'InvalidSellerSignature
  | BidTerms'Error'InvalidBidderSignature
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe BidTerms'Error where
  universe = universeGeneric

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix BidTerms'Error where
  errorCodePrefix = const "BITE"
