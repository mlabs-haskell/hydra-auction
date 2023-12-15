module HydraAuction.Onchain.Types.AuctionEscrowState (
  AuctionEscrowState (..),
  validateAuctionEscrowTransitionToStartBidding,
  validateAuctionEscrowTransitionToAuctionConcluded,
) where

import PlutusTx.Prelude

import PlutusTx qualified

import HydraAuction.Error.Types.AuctionEscrowState (
  AuctionEscrowState'Error (..),
 )
import HydraAuction.Onchain.Lib.Error (eCode, err)

data AuctionEscrowState
  = AuctionAnnounced
  | BiddingStarted
  | AuctionConcluded

instance Eq AuctionEscrowState where
  AuctionAnnounced == AuctionAnnounced = True
  BiddingStarted == BiddingStarted = True
  AuctionConcluded == AuctionConcluded = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''AuctionEscrowState

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
