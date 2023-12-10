module HydraAuction.Error.Onchain.Validators.AuctionMetadata (
  AuctionMetadata'Error (..),
) where

import Prelude

import Data.Universe (Universe)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))

data AuctionMetadata'Error
  = AuctionMetadata'Error'TooManyOwnScriptInputs
  | AuctionMetadata'Error'AuctionTokensNotBurnedExactly
  | AuctionMetadata'Error'MissingMetadataInput
  deriving stock (Bounded, Enum, Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe AuctionMetadata'Error

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix AuctionMetadata'Error where
  errorCodePrefix = const "AUMD"
