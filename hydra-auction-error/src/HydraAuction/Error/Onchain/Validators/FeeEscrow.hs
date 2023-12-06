module HydraAuction.Error.Onchain.Validators.FeeEscrow (
  FeeEscrow'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))

data FeeEscrow'Error
  = FeeEscrow'Error'MissingOwnInput
  | FeeEscrow'Error'TooManyOwnScriptInputs
  | FeeEscrow'Error'UnexpectedMintOrBurn
  | FeeEscrow'Error'InsufficientDelegateFeePayments
  deriving stock (Eq, Generic, Show)

instance ToErrorCode FeeEscrow'Error where
  toErrorCode = \case
    FeeEscrow'Error'MissingOwnInput ->
      "FeeEscrow01"
    FeeEscrow'Error'TooManyOwnScriptInputs ->
      "FeeEscrow02"
    FeeEscrow'Error'UnexpectedMintOrBurn ->
      "FeeEscrow03"
    FeeEscrow'Error'InsufficientDelegateFeePayments ->
      "FeeEscrow04"
