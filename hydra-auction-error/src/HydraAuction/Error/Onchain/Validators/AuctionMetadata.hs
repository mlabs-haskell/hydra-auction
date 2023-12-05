module HydraAuction.Error.Onchain.Validators.AuctionMetadata (
  AuctionMetadata'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))

data AuctionMetadata'Error
  = AuctionMetadata'Error'TooManyOwnScriptInputs
  | AuctionMetadata'Error'OwnInputMissingMetadataToken
  | AuctionMetadata'Error'AuctionTokensNotBurnedExactly
  | AuctionMetadata'Error'MissingOwnInput
  | AuctionMetadata'Error'FailedToDecodeOwnDatum
  | AuctionMetadata'Error'MissingOwnDatum
  deriving stock (Eq, Generic, Show)

instance ToErrorCode AuctionMetadata'Error where
  toErrorCode = \case
    AuctionMetadata'Error'TooManyOwnScriptInputs ->
      "AuctionMetadata01"
    AuctionMetadata'Error'OwnInputMissingMetadataToken ->
      "AuctionMetadata02"
    AuctionMetadata'Error'AuctionTokensNotBurnedExactly ->
      "AuctionMetadata03"
    AuctionMetadata'Error'MissingOwnInput ->
      "AuctionMetadata04"
    AuctionMetadata'Error'FailedToDecodeOwnDatum ->
      "AuctionMetadata05"
    AuctionMetadata'Error'MissingOwnDatum ->
      "AuctionMetadata06"
