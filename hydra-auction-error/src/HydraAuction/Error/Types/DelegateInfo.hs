module HydraAuction.Error.Types.DelegateInfo (
  DelegateInfo'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))

data DelegateInfo'Error
  = DelegateInfo'Error'NoDelegates
  deriving stock (Eq, Generic, Show)

instance ToErrorCode DelegateInfo'Error where
  toErrorCode = \case
    DelegateInfo'Error'NoDelegates ->
      "DelegateInfo01"
