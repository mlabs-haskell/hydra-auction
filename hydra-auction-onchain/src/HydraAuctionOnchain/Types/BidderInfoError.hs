module HydraAuctionOnchain.Types.BidderInfoError (
  BidderInfo'Error (..),
) where

import HydraAuctionOnchain.Lib.Error (ToErrorCode (..))

data BidderInfo'Error
  = BidderInfo'Error'BidderVkPkhMismatch

instance ToErrorCode BidderInfo'Error where
  toErrorCode = \case
    BidderInfo'Error'BidderVkPkhMismatch ->
      "BI01"
