module HydraAuction.Error.Onchain.Validators.AuctionMetadata (
  AuctionMetadata'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))

data AuctionMetadata'Error
  = AuctionMetadata'Error'TooManyOwnScriptInputs
  | AuctionMetadata'Error'AuctionTokensNotBurnedExactly
  | AuctionMetadata'Error'MissingOwnInput
  | AuctionMetadata'Error'MissingOwnDatum
  | AuctionMetadata'Error'FailedToDecodeOwnDatum
  deriving stock (Eq, Generic, Show)

instance ToErrorCode AuctionMetadata'Error where
  toErrorCode = \case
    AuctionMetadata'Error'TooManyOwnScriptInputs ->
      "AuctionMetadata01"
    AuctionMetadata'Error'AuctionTokensNotBurnedExactly ->
      "AuctionMetadata02"
    AuctionMetadata'Error'MissingOwnInput ->
      "AuctionMetadata03"
    AuctionMetadata'Error'MissingOwnDatum ->
      "AuctionMetadata04"
    AuctionMetadata'Error'FailedToDecodeOwnDatum ->
      "AuctionMetadata05"
