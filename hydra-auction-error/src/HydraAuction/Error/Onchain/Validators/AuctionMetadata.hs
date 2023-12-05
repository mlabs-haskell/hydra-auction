module HydraAuction.Error.Onchain.Validators.AuctionMetadata (
  AuctionMetadata'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))

data AuctionMetadata'Error
  = AuctionMetadata'Error'MissingOwnInput
  | AuctionMetadata'Error'TooManyOwnScriptInputs
  | AuctionMetadata'Error'OwnInputMissingMetadataToken
  | AuctionMetadata'Error'AuctionTokensNotBurnedExactly
  deriving stock (Eq, Generic, Show)

instance ToErrorCode AuctionMetadata'Error where
  toErrorCode = \case
    AuctionMetadata'Error'MissingOwnInput ->
      "AuctionMetadata01"
    AuctionMetadata'Error'TooManyOwnScriptInputs ->
      "AuctionMetadata02"
    AuctionMetadata'Error'OwnInputMissingMetadataToken ->
      "AuctionMetadata03"
    AuctionMetadata'Error'AuctionTokensNotBurnedExactly ->
      "AuctionMetadata04"
