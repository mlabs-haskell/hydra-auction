module HydraAuction.Error.Onchain.MintingPolicies.AuctionMp (
  AuctionMp'Error (..),
) where

import Prelude

import Data.Universe (Universe (..), universeGeneric)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))

import HydraAuction.Error.Types.AuctionTerms (AuctionTerms'Error)

data AuctionMp'Error
  = AuctionMp'MI'Error'MissingUtxoNonceInput
  | AuctionMp'MI'Error'AuctionInfoMismatchedToken
  | AuctionMp'MI'Error'InvalidAuctionTerms [AuctionTerms'Error]
  | AuctionMp'MI'Error'AuctionTokensNotMinted
  | AuctionMp'MI'Error'FailedToDecodeMetadataDatum
  | AuctionMp'MI'Error'MissingMetadataOutput
  | AuctionMp'BU'Error'AuctionTokensNotBurned
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe AuctionMp'Error where
  universe = universeGeneric

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix AuctionMp'Error where
  errorCodePrefix = const "AUMP"
