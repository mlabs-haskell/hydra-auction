module HydraAuction.Error.Onchain.MintingPolicies.Auction (
  AuctionMint'Error (..),
  AuctionBurn'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))

data AuctionMint'Error
  = AuctionMint'Error'AuctionTokensNotMinted
  | AuctionMint'Error'MissingUtxoNonceInput
  | AuctionMint'Error'MissingMetadataOutput
  | AuctionMint'Error'TooManyMetadataOutputs
  | AuctionMint'Error'MetadataOutputMissingDatum
  | AuctionMint'Error'FailedToDecodeMetadataDatum
  | AuctionMint'Error'AuctionInfoMismatchedToken
  | AuctionMint'Error'MetadataOutputMissingToken
  deriving stock (Eq, Generic, Show)

instance ToErrorCode AuctionMint'Error where
  toErrorCode = \case
    AuctionMint'Error'AuctionTokensNotMinted ->
      "AuctionMint01"
    AuctionMint'Error'MissingUtxoNonceInput ->
      "AuctionMint02"
    AuctionMint'Error'MissingMetadataOutput ->
      "AuctionMint03"
    AuctionMint'Error'TooManyMetadataOutputs ->
      "AuctionMint04"
    AuctionMint'Error'MetadataOutputMissingDatum ->
      "AuctionMint05"
    AuctionMint'Error'FailedToDecodeMetadataDatum ->
      "AuctionMint06"
    AuctionMint'Error'AuctionInfoMismatchedToken ->
      "AuctionMint07"
    AuctionMint'Error'MetadataOutputMissingToken ->
      "AuctionMint08"

data AuctionBurn'Error
  = AuctionBurn'Error'AuctionTokensNotBurned
  deriving stock (Eq, Generic, Show)

instance ToErrorCode AuctionBurn'Error where
  toErrorCode = \case
    AuctionBurn'Error'AuctionTokensNotBurned ->
      "AuctionBurn01"
