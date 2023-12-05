module HydraAuction.Error.Types.BidderInfo (
  BidderInfo'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))

data BidderInfo'Error
  = BidderInfo'Error'BidderVkPkhMismatch
  deriving stock (Eq, Generic, Show)

instance ToErrorCode BidderInfo'Error where
  toErrorCode = \case
    BidderInfo'Error'BidderVkPkhMismatch ->
      "BidderInfo01"
