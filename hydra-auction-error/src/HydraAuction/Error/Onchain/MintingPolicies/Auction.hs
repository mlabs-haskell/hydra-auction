module HydraAuction.Error.Onchain.MintingPolicies.Auction (
  AuctionMP'Error (..),
) where

import Prelude

import Data.Universe (Universe (..), universeGeneric)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))

import HydraAuction.Error.Types.AuctionTerms (AuctionTerms'Error)

data AuctionMP'Error
  = AuctionMP'MI'Error'MissingUtxoNonceInput
  | AuctionMP'MI'Error'AuctionInfoMismatchedToken
  | AuctionMP'MI'Error'InvalidAuctionTerms [AuctionTerms'Error]
  | AuctionMP'MI'Error'AuctionTokensNotMinted
  | AuctionMP'MI'Error'FailedToDecodeMetadataDatum
  | AuctionMP'MI'Error'MissingMetadataOutput
  | AuctionMP'BU'Error'AuctionTokensNotBurned
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe AuctionMP'Error where
  universe = universeGeneric

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix AuctionMP'Error where
  errorCodePrefix = const "AUMP"
