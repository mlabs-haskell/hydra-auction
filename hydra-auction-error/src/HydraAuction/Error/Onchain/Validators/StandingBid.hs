module HydraAuction.Error.Onchain.Validators.StandingBid (
  StandingBid'Error (..),
) where

import Prelude

import Data.Universe (Universe (..), universeGeneric)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))
import HydraAuction.Error.Types.AuctionState (
  NewBid'Error,
 )

data StandingBid'Error
  = -- Common errors
    StandingBid'Error'MissingOwnInput
  | StandingBid'Error'TooManyOwnScriptInputs
  | StandingBid'Error'UnexpectedTokensMintedBurned
  | -- NewBid errors
    StandingBid'NB'Error'OwnInputMissingToken
  | StandingBid'NB'Error'InvalidNewBidState [NewBid'Error]
  | StandingBid'NB'Error'IncorrectValidityInterval
  | StandingBid'NB'Error'FailedToDecodeNewBid
  | StandingBid'NB'Error'OwnOutputDatumNotInline
  | StandingBid'NB'Error'MissingOwnOutput
  | -- MoveToHydra errors
    StandingBid'MH'Error'MissingDelegateSignatures
  | StandingBid'MH'Error'IncorrectValidityInterval
  | -- ConcludeAuction errors
    StandingBid'CA'Error'OwnInputMissingToken
  | StandingBid'CA'Error'MissingAuctionStateToken
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe StandingBid'Error where
  universe = universeGeneric

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix StandingBid'Error where
  errorCodePrefix = const "STBD"
