module HydraAuction.Error.Onchain.Validators.FeeEscrow (
  FeeEscrow'Error (..),
) where

import Prelude

import Data.Universe (Universe)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))

data FeeEscrow'Error
  = FeeEscrow'Error'MissingOwnInput
  | FeeEscrow'Error'TooManyOwnScriptInputs
  | FeeEscrow'Error'UnexpectedMintOrBurn
  | FeeEscrow'Error'InsufficientDelegateFeePayments
  deriving stock (Bounded, Enum, Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe FeeEscrow'Error

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix FeeEscrow'Error where
  errorCodePrefix = const "FEES"
