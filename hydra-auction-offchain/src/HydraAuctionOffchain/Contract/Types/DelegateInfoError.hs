module HydraAuctionOffchain.Contract.Types.DelegateInfoError (
  DelegateInfo'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

data DelegateInfo'Error
  = DelegateInfo'Error'NoDelegates
  deriving stock (Eq, Generic, Show)
