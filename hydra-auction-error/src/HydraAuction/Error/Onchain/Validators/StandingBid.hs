module HydraAuction.Error.Onchain.Validators.StandingBid (
  StandingBid'Error (..),
  StandingBid'NB'Error (..),
  StandingBid'MH'Error (..),
  StandingBid'CA'Error (..),
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Error (ToErrorCode (..))

import HydraAuction.Error.Types.AuctionState (
  NewBid'Error,
 )

data StandingBid'Error
  = StandingBid'Error'MissingOwnInput
  | StandingBid'Error'TooManyOwnScriptInputs
  | StandingBid'Error'UnexpectedTokensMintedBurned
  deriving stock (Eq, Generic, Show)

instance ToErrorCode StandingBid'Error where
  toErrorCode = \case
    StandingBid'Error'MissingOwnInput ->
      "StandingBid01"
    StandingBid'Error'TooManyOwnScriptInputs ->
      "StandingBid02"
    StandingBid'Error'UnexpectedTokensMintedBurned ->
      "StandingBid03"

data StandingBid'NB'Error
  = StandingBid'NB'Error'OwnInputMissingToken
  | StandingBid'NB'Error'InvalidNewBidState [NewBid'Error]
  | StandingBid'NB'Error'IncorrectValidityInterval
  | StandingBid'NB'Error'FailedToDecodeNewBid
  | StandingBid'NB'Error'OwnOutputDatumNotInline
  | StandingBid'NB'Error'MissingOwnOutput
  deriving stock (Eq, Generic, Show)

instance ToErrorCode StandingBid'NB'Error where
  toErrorCode = \case
    StandingBid'NB'Error'OwnInputMissingToken ->
      "StandingBid_NB01"
    StandingBid'NB'Error'InvalidNewBidState _ ->
      "StandingBid_NB02"
    StandingBid'NB'Error'IncorrectValidityInterval ->
      "StandingBid_NB03"
    StandingBid'NB'Error'FailedToDecodeNewBid ->
      "StandingBid_NB04"
    StandingBid'NB'Error'OwnOutputDatumNotInline ->
      "StandingBid_NB05"
    StandingBid'NB'Error'MissingOwnOutput ->
      "StandingBid_NB06"

data StandingBid'MH'Error
  = StandingBid'MH'Error'MissingDelegateSignatures
  | StandingBid'MH'Error'IncorrectValidityInterval
  deriving stock (Eq, Generic, Show)

instance ToErrorCode StandingBid'MH'Error where
  toErrorCode = \case
    StandingBid'MH'Error'MissingDelegateSignatures ->
      "StandingBid_MH01"
    StandingBid'MH'Error'IncorrectValidityInterval ->
      "StandingBid_MH02"

data StandingBid'CA'Error
  = StandingBid'CA'Error'OwnInputMissingToken
  | StandingBid'CA'Error'MissingAuctionStateToken
  deriving stock (Eq, Generic, Show)

instance ToErrorCode StandingBid'CA'Error where
  toErrorCode = \case
    StandingBid'CA'Error'OwnInputMissingToken ->
      "StandingBid_CA01"
    StandingBid'CA'Error'MissingAuctionStateToken ->
      "StandingBid_CA02"
