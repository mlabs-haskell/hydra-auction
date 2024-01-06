module HydraAuction.Onchain.Types.StandingBidState (
  StandingBidState (..),
  validateNewBid,
  bidderLost,
  bidderWon,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V2 (CurrencySymbol)
import PlutusTx qualified

import HydraAuction.Error.Types.StandingBidState (
  NewBid'Error (..),
 )
import HydraAuction.Onchain.Lib.Error (eCode, err)
import HydraAuction.Onchain.Types.AuctionTerms (
  AuctionTerms (..),
 )
import HydraAuction.Onchain.Types.BidTerms (
  BidTerms (..),
  bidderMadeBid,
  validateBidTerms,
 )
import HydraAuction.Onchain.Types.BidderInfo (
  BidderInfo (..),
 )

newtype StandingBidState = StandingBidState
  { standingBidState :: Maybe BidTerms
  }

{- FOURMOLU_DISABLE -}
instance Eq StandingBidState where
  ( StandingBidState x1) ==
   (StandingBidState y1) =
    x1 == y1
{- FOURMOLU_ENABLE -}

PlutusTx.unstableMakeIsData ''StandingBidState

-- -------------------------------------------------------------------------
-- Bidder predicates
-- -------------------------------------------------------------------------

bidderLost :: StandingBidState -> BidderInfo -> Bool
bidderLost StandingBidState {..} x =
  maybe True (`bidderMadeBid` x) standingBidState
--
{-# INLINEABLE bidderLost #-}

bidderWon :: StandingBidState -> BidderInfo -> Bool
bidderWon sbs x = not $ bidderLost sbs x
--
{-# INLINEABLE bidderWon #-}

-- -------------------------------------------------------------------------
-- Standing bid state transition validation
-- -------------------------------------------------------------------------

validateNewBid ::
  AuctionTerms ->
  CurrencySymbol ->
  StandingBidState ->
  StandingBidState ->
  Bool
validateNewBid auTerms auctionCs oldBidState StandingBidState {..}
  | Just newTerms <- standingBidState =
      validateNewBidTerms auTerms auctionCs newTerms
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
