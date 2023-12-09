module HydraAuction.Error.Onchain.MintingPolicies.Auction (
  AuctionMP'Error (..),
) where

import Prelude

import Data.Universe (Universe (..), universeGeneric)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))

import HydraAuction.Error.Types.AuctionTerms (AuctionTerms'Error)

data AuctionMP'Error
  = AuctionMP'Error'MI'MissingUtxoNonceInput
  | AuctionMP'Error'MI'AuctionInfoMismatchedToken
  | AuctionMP'Error'MI'MetadataOutputMissingToken
  | AuctionMP'Error'MI'InvalidAuctionTerms [AuctionTerms'Error]
  | AuctionMP'Error'MI'AuctionTokensNotMinted
  | AuctionMP'Error'MI'FailedToDecodeMetadataDatum
  | AuctionMP'Error'MI'MetadataOutputMissingDatum
  | AuctionMP'Error'MI'MissingMetadataOutput
  | AuctionMP'Error'MI'TooManyMetadataOutputs
  | AuctionMP'Error'BU'AuctionTokensNotBurned
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
