module HydraAuction.Error.Onchain.Validators.AuctionEscrow (
  AuctionEscrow'Error (..),
) where

import Prelude

import Data.Universe (Universe)
import GHC.Generics (Generic)

import HydraAuction.Error (ErrorCodePrefix (..))

data AuctionEscrow'Error
  = -- Common errors
    AuctionEscrow'Error'TooManyOwnScriptInputs
  | AuctionEscrow'Error'MissingAuctionEscrowInput
  | -- StartBidding errors
    AuctionEscrow'SB'Error'InvalidAuctionStateTransition
  | AuctionEscrow'SB'Error'NewAuctionEscrowStateInvalid
  | AuctionEscrow'SB'Error'InitialBidStateInvalid
  | AuctionEscrow'SB'Error'IncorrectValidityInterval
  | AuctionEscrow'SB'Error'MissingSellerSignature
  | AuctionEscrow'SB'Error'UnexpectedTokensMintedBurned
  | AuctionEscrow'SB'Error'UndecodedAuctionEscrowDatum
  | AuctionEscrow'SB'Error'MissingAuctionEscrowOutput
  | AuctionEscrow'SB'Error'UndecodedInitialBid
  | AuctionEscrow'SB'Error'MissingStandingBidOutput
  | -- BuyerBuys errors
    AuctionEscrow'BB'Error'InvalidAuctionStateTransition
  | AuctionEscrow'BB'Error'AuctionEscrowOutputMissingTokens
  | AuctionEscrow'BB'Error'BidTermsInvalid
  | AuctionEscrow'BB'Error'AuctionLotNotPaidToBuyer
  | AuctionEscrow'BB'Error'SellerPaymentIncorrect
  | AuctionEscrow'BB'Error'PaymentToFeeEscrowIncorrect
  | AuctionEscrow'BB'Error'IncorrectValidityInterval
  | AuctionEscrow'BB'Error'UnexpectedTokensMintedBurned
  | AuctionEscrow'BB'Error'UndecodedAuctionEscrowDatum
  | AuctionEscrow'BB'Error'MissingAuctionEscrowOutput
  | AuctionEscrow'BB'Error'EmptyStandingBid
  | AuctionEscrow'BB'Error'UndecodedStandingBid
  | AuctionEscrow'BB'Error'MissingStandingBidOutput
  | -- SellerReclaims errors
    AuctionEscrow'SR'Error'InvalidAuctionStateTransition
  | AuctionEscrow'SR'Error'AuctionEscrowOutputMissingTokens
  | AuctionEscrow'SR'Error'PaymentToSellerIncorrect
  | AuctionEscrow'SR'Error'PaymentToFeeEscrowIncorrect
  | AuctionEscrow'SR'Error'IncorrectValidityInterval
  | AuctionEscrow'SR'Error'UnexpectedTokensMintedBurned
  | AuctionEscrow'SR'Error'UndecodedAuctionEscrowDatum
  | AuctionEscrow'SR'Error'MissingAuctionEscrowOutput
  | -- ConcludeAuction errors
    AuctionEscrow'CA'Error'AuctionIsNotConcluded
  | AuctionEscrow'CA'Error'AuctionEscrowInputMissingTokens
  | AuctionEscrow'CA'Error'AuctionTokensNotBurnedExactly
  | AuctionEscrow'CA'Error'IncorrectValidityInterval
  deriving stock (Bounded, Enum, Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Universe
-- -------------------------------------------------------------------------

instance Universe AuctionEscrow'Error

-- -------------------------------------------------------------------------
-- Error code prefix
-- -------------------------------------------------------------------------

instance ErrorCodePrefix AuctionEscrow'Error where
  errorCodePrefix = const "AUES"
