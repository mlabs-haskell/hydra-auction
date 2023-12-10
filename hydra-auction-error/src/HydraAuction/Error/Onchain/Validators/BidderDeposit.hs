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
  | -- DepositUsedByWinner errors
    BidderDeposit'BW'Error'BidderIsNotBuyer
  | BidderDeposit'BW'Error'RedeemerNotBidderBuys
  | BidderDeposit'BW'Error'UndecodedAuctionRedeemer
  | BidderDeposit'BW'Error'MissingAuctionEscrowInput
  | -- DepositClaimedBySeller errors
    BidderDeposit'BS'Error'MismatchAuctionRedeemer
  | BidderDeposit'BS'Error'BidderNotWinner
  | BidderDeposit'BS'Error'NoSellerConsent
  | BidderDeposit'BS'Error'UndecodedAuctionRedeemer
  | BidderDeposit'BS'Error'MissingAuctionEscrowInput
  | BidderDeposit'BS'Error'UndecodedBidState
  | BidderDeposit'BS'Error'MissingStandingBidInput
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
