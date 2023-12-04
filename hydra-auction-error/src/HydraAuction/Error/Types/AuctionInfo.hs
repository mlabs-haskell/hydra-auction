module HydraAuction.Error.Types.AuctionInfo (
  AuctionInfo'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))
import HydraAuction.Error.Types.AuctionTerms (AuctionTerms'Error)

data AuctionInfo'Error
  = AuctionInfo'Error'InvalidAuctionIdCurrencySymbol
  | AuctionInfo'Error'InvalidAuctionTerms [AuctionTerms'Error]
  | AuctionInfo'Error'InvalidScriptAddresses
  deriving stock (Eq, Generic, Show)

instance ToErrorCode AuctionInfo'Error where
  toErrorCode = \case
    AuctionInfo'Error'InvalidAuctionIdCurrencySymbol ->
      "AuctionInfo01"
    AuctionInfo'Error'InvalidAuctionTerms _ ->
      "AuctionInfo02"
    AuctionInfo'Error'InvalidScriptAddresses ->
      "AuctionInfo03"
