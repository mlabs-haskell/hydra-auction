module HydraAuction.Onchain.Types.AuctionState (
  AuctionEscrowState (..),
  StandingBidState (..),
  validateAuctionEscrowTransitionToStartBidding,
  validateAuctionEscrowTransitionToAuctionConcluded,
  validateNewBid,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1 (CurrencySymbol)
import PlutusTx qualified

import HydraAuction.Onchain.Lib.Error (eCode, err)

import HydraAuction.Error.Types.AuctionState (
  AuctionEscrowState'Error (..),
  NewBid'Error (..),
 )
import HydraAuction.Onchain.Types.AuctionTerms (
  AuctionTerms (..),
 )
import HydraAuction.Onchain.Types.BidTerms (
  BidTerms (..),
  validateBidTerms,
 )

data AuctionEscrowState
  = AuctionAnnounced
  | BiddingStarted
  | AuctionConcluded

newtype StandingBidState = StandingBidState
  { standingBidState :: Maybe BidTerms
  }

instance Eq AuctionEscrowState where
  AuctionAnnounced == AuctionAnnounced = True
  AuctionAnnounced == BiddingStarted = False
  AuctionAnnounced == AuctionConcluded = False
  --
  BiddingStarted == AuctionAnnounced = False
  BiddingStarted == BiddingStarted = True
  BiddingStarted == AuctionConcluded = False
  --
  AuctionConcluded == AuctionAnnounced = False
  AuctionConcluded == BiddingStarted = False
  AuctionConcluded == AuctionConcluded = True

instance Eq StandingBidState where
  (StandingBidState x1)
    == (StandingBidState y1) =
      x1 == y1

PlutusTx.unstableMakeIsData ''AuctionEscrowState
PlutusTx.unstableMakeIsData ''StandingBidState

-- -------------------------------------------------------------------------
-- Auction escrow state transition validation
-- -------------------------------------------------------------------------

validateAuctionEscrowTransitionToStartBidding ::
  AuctionEscrowState ->
  AuctionEscrowState ->
  Bool
validateAuctionEscrowTransitionToStartBidding oldState newState =
  oldStateIsCorrect && newStateIsCorrect
  where
    oldStateIsCorrect =
      (oldState == AuctionAnnounced)
        `err` $(eCode AuctionEscrowState'SB'Error'InvalidOldState)
    newStateIsCorrect =
      (newState == BiddingStarted)
        `err` $(eCode AuctionEscrowState'SB'Error'InvalidNewState)
--
{-# INLINEABLE validateAuctionEscrowTransitionToStartBidding #-}

validateAuctionEscrowTransitionToAuctionConcluded ::
  AuctionEscrowState ->
  AuctionEscrowState ->
  Bool
validateAuctionEscrowTransitionToAuctionConcluded oldState newState =
  oldStateIsCorrect && newStateIsCorrect
  where
    oldStateIsCorrect =
      (oldState == BiddingStarted)
        `err` $(eCode AuctionEscrowState'AC'Error'InvalidOldState)
    newStateIsCorrect =
      (newState == AuctionConcluded)
        `err` $(eCode AuctionEscrowState'AC'Error'InvalidNewState)
--
{-# INLINEABLE validateAuctionEscrowTransitionToAuctionConcluded #-}

-- -------------------------------------------------------------------------
-- Standing bid state transition validation
-- -------------------------------------------------------------------------

validateNewBid ::
  AuctionTerms ->
  CurrencySymbol ->
  StandingBidState ->
  StandingBidState ->
  Bool
validateNewBid auTerms auctionId oldBidState StandingBidState {..}
  | Just newTerms <- standingBidState =
      validateNewBidTerms auTerms auctionId newTerms
        && validateCompareBids auTerms oldBidState newTerms
  | otherwise =
      --
      -- The new bid state should not be empty.
      False
        `err` $(eCode NewBid'Error'EmptyNewBid)
--
{-# INLINEABLE validateNewBid #-}

validateNewBidTerms ::
  AuctionTerms ->
  CurrencySymbol ->
  BidTerms ->
  Bool
validateNewBidTerms =
  --
  -- The new bid terms are valid.
  validateBidTerms
--
{-# INLINEABLE validateNewBidTerms #-}

validateCompareBids ::
  AuctionTerms ->
  StandingBidState ->
  BidTerms ->
  Bool
validateCompareBids auTerms StandingBidState {..} newTerms
  | Just oldTerms <- standingBidState =
      validateBidIncrement auTerms oldTerms newTerms
  | otherwise =
      validateStartingBid auTerms newTerms
--
{-# INLINEABLE validateCompareBids #-}

validateBidIncrement ::
  AuctionTerms ->
  BidTerms ->
  BidTerms ->
  Bool
validateBidIncrement AuctionTerms {..} oldTerms newTerms =
  --
  -- The difference between the old and new bid price is
  -- no smaller than the auction's minimum bid increment.
  (bt'BidPrice oldTerms + at'MinBidIncrement <= bt'BidPrice newTerms)
    `err` $(eCode NewBid'Error'InvalidBidIncrement)
--
{-# INLINEABLE validateBidIncrement #-}

validateStartingBid ::
  AuctionTerms ->
  BidTerms ->
  Bool
validateStartingBid AuctionTerms {..} BidTerms {..} =
  --
  -- The first bid's price is
  -- no smaller than the auction's starting price.
  (at'StartingBid <= bt'BidPrice)
    `err` $(eCode NewBid'Error'InvalidStartingBid)
--
{-# INLINEABLE validateStartingBid #-}
