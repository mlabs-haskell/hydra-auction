module HydraAuction.Error.Types.DelegateInfo (
  DelegateInfo'Error (..),
) where

import Prelude

import Data.Universe (Universe)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))

data DelegateInfo'Error
  = DelegateInfo'Error'NoDelegates
  deriving stock (Bounded, Enum, Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe DelegateInfo'Error

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix DelegateInfo'Error where
  errorCodePrefix = const "DEIN"
