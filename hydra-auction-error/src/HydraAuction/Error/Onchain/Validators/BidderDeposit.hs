module HydraAuction.Error.Onchain.Validators.BidderDeposit (
  BidderDeposit'Error (..),
) where

import Prelude

import Data.Universe (Universe)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))

data BidderDeposit'Error
  = -- Common errors
    BidderDeposit'Error'TooManyOwnScriptInputs
  | BidderDeposit'Error'UnexpectedMintOrBurn
  | BidderDeposit'Error'MissingOwnInput
  | -- DepositClaimedBySeller errors
    BidderDeposit'CA'Error'AuctionNotConcluding
  | BidderDeposit'CA'Error'BidderNotWinner
  | BidderDeposit'CA'Error'UndecodedAuctionRedeemer
  | BidderDeposit'CA'Error'MissingAuctionEscrowInput
  | BidderDeposit'CA'Error'UndecodedBidState
  | BidderDeposit'CA'Error'MissingStandingBidInput
  | -- DepositReclaimedByLoser errors
    BidderDeposit'BL'Error'BidderNotLoser
  | BidderDeposit'BL'Error'ValidityIntervalIncorrect
  | BidderDeposit'BL'Error'NoBidderConsent
  | BidderDeposit'BL'Error'UndecodedBidState
  | BidderDeposit'BL'Error'MissingStandingBidInput
  | -- DepositReclaimedAuctionConcluded errors
    BidderDeposit'AC'Error'AuctionNotConcluded
  | BidderDeposit'AC'Error'ValidityIntervalIncorrect
  | BidderDeposit'AC'Error'NoBidderConsent
  | BidderDeposit'AC'Error'UndecodedAuctionState
  | BidderDeposit'AC'Error'MissingAuctionRefInput
  | -- DepositCleanup errors
    BidderDeposit'DC'Error'ValidityIntervalIncorrect
  | BidderDeposit'DC'Error'NoBidderConsent
  deriving stock (Bounded, Enum, Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe BidderDeposit'Error

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix BidderDeposit'Error where
  errorCodePrefix = const "BIDE"
