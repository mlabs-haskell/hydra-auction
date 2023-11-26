module HydraAuctionOnchain.Types.DelegateInfoError (
  DelegateInfo'Error (..),
) where

import HydraAuctionOnchain.Lib.Error (ToErrorCode (..))

data DelegateInfo'Error
  = DelegateInfo'Error'NoDelegates

instance ToErrorCode DelegateInfo'Error where
  toErrorCode = \case
    DelegateInfo'Error'NoDelegates ->
      "DI01"
