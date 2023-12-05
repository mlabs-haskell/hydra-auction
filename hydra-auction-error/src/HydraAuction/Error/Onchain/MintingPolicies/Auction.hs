module HydraAuction.Error.Onchain.MintingPolicies.Auction (
  AuctionMint'Error (..),
  AuctionBurn'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))

import HydraAuction.Error.Types.AuctionTerms (AuctionTerms'Error)

data AuctionMint'Error
  = AuctionMint'Error'MissingUtxoNonceInput
  | AuctionMint'Error'AuctionInfoMismatchedToken
  | AuctionMint'Error'MetadataOutputMissingToken
  | AuctionMint'Error'InvalidAuctionTerms [AuctionTerms'Error]
  | AuctionMint'Error'AuctionTokensNotMinted
  | AuctionMint'Error'FailedToDecodeMetadataDatum
  | AuctionMint'Error'MetadataOutputMissingDatum
  | AuctionMint'Error'MissingMetadataOutput
  | AuctionMint'Error'TooManyMetadataOutputs
  deriving stock (Eq, Generic, Show)

instance ToErrorCode AuctionMint'Error where
  toErrorCode = \case
    AuctionMint'Error'MissingUtxoNonceInput ->
      "AuctionMint01"
    AuctionMint'Error'AuctionInfoMismatchedToken ->
      "AuctionMint02"
    AuctionMint'Error'MetadataOutputMissingToken ->
      "AuctionMint03"
    AuctionMint'Error'InvalidAuctionTerms _ ->
      "AuctionMint04"
    AuctionMint'Error'AuctionTokensNotMinted ->
      "AuctionMint05"
    AuctionMint'Error'FailedToDecodeMetadataDatum ->
      "AuctionMint06"
    AuctionMint'Error'MetadataOutputMissingDatum ->
      "AuctionMint07"
    AuctionMint'Error'MissingMetadataOutput ->
      "AuctionMint08"
    AuctionMint'Error'TooManyMetadataOutputs ->
      "AuctionMint09"

data AuctionBurn'Error
  = AuctionBurn'Error'AuctionTokensNotBurned
  deriving stock (Eq, Generic, Show)

instance ToErrorCode AuctionBurn'Error where
  toErrorCode = \case
    AuctionBurn'Error'AuctionTokensNotBurned ->
      "AuctionBurn01"
