module HydraAuctionOffchain.Contract.Types.AuctionState (
  AuctionEscrowState (..),
  StandingBidState (..),
  Buyer'Error (..),
  NewBid'Error (..),
  sellerPayout,
  validateBuyer,
  validateNewBid,
) where

import GHC.Generics (Generic)
import Prelude

import Data.Validation (Validation (..))

import HydraAuctionOffchain.Lib.Validation (err, errWith)

import HydraAuctionOffchain.Contract.Types.AuctionTerms (
  AuctionTerms (..),
  totalAuctionFees,
 )
import HydraAuctionOffchain.Contract.Types.BidTerms (
  BidTerms (..),
  validateBidTerms,
 )
import HydraAuctionOffchain.Contract.Types.BidderInfo (BidderInfo (..))

import Cardano.Api.Shelley (
  Lovelace (..),
  PolicyId (..),
 )

import HydraAuctionOffchain.Lib.Crypto (
  Hash,
  PaymentKey,
 )

import HydraAuctionOffchain.Contract.Types.AuctionStateError (
  Buyer'Error (..),
  NewBid'Error (..),
 )

data AuctionEscrowState
  = AuctionAnnounced
  | BiddingStarted
  | AuctionConcluded
  deriving stock (Eq, Generic, Show)

newtype StandingBidState = StandingBidState
  {standingBidState :: Maybe BidTerms}
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- New bid validation
-- -------------------------------------------------------------------------

validateNewBid ::
  AuctionTerms ->
  PolicyId ->
  StandingBidState ->
  StandingBidState ->
  Validation [NewBid'Error] ()
validateNewBid auTerms auctionId oldBidState StandingBidState {..}
  | Just newTerms <- standingBidState =
      validateNewBidTerms auTerms auctionId newTerms
        <> validateCompareBids auTerms oldBidState newTerms
  | otherwise =
      --
      -- (NB01) The new bid state should not be empty.
      False
        `err` NewBid'Error'EmptyNewBid

validateNewBidTerms ::
  AuctionTerms ->
  PolicyId ->
  BidTerms ->
  Validation [NewBid'Error] ()
validateNewBidTerms auTerms auctionId newTerms =
  --
  -- (NB02) The new bid terms are valid.
  validateBidTerms auTerms auctionId newTerms
    `errWith` NewBid'Error'InvalidNewBidTerms

validateCompareBids ::
  AuctionTerms ->
  StandingBidState ->
  BidTerms ->
  Validation [NewBid'Error] ()
validateCompareBids auTerms StandingBidState {..} newTerms
  | Just oldTerms <- standingBidState =
      validateBidIncrement auTerms oldTerms newTerms
  | otherwise =
      validateStartingBid auTerms newTerms

validateBidIncrement ::
  AuctionTerms ->
  BidTerms ->
  BidTerms ->
  Validation [NewBid'Error] ()
validateBidIncrement AuctionTerms {..} oldTerms newTerms =
  --
  -- (NB03) The difference between the old and new bid price is
  -- no smaller than the auction's minimum bid increment.
  (bt'BidPrice oldTerms + at'MinBidIncrement <= bt'BidPrice newTerms)
    `err` NewBid'Error'InvalidBidIncrement

validateStartingBid ::
  AuctionTerms ->
  BidTerms ->
  Validation [NewBid'Error] ()
validateStartingBid AuctionTerms {..} BidTerms {..} =
  --
  -- (NB04) The first bid's price is
  -- no smaller than the auction's starting price.
  (at'StartingBid <= bt'BidPrice)
    `err` NewBid'Error'InvalidStartingBid

-- -------------------------------------------------------------------------
-- Buyer validation
-- -------------------------------------------------------------------------

validateBuyer ::
  AuctionTerms ->
  PolicyId ->
  StandingBidState ->
  Hash PaymentKey ->
  Validation [Buyer'Error] ()
validateBuyer auTerms auctionId StandingBidState {..} buyer
  | Just bidTerms@BidTerms {..} <- standingBidState
  , BidderInfo {..} <- bt'Bidder =
      --
      -- (BU01) The buyer's hashed payment verification key corresponds
      -- to the bidder's payment verification key.
      (buyer == bi'BidderPkh)
        `err` Buyer'Error'BuyerVkPkhMismatch
        --
        -- (BU02) The bid terms are valid.
        <> validateBidTerms auTerms auctionId bidTerms
        `errWith` Buyer'Error'InvalidBidTerms
  | otherwise = Failure [Buyer'Error'EmptyStandingBid]

-- -------------------------------------------------------------------------
-- Seller payout
-- -------------------------------------------------------------------------

sellerPayout :: AuctionTerms -> StandingBidState -> Lovelace
sellerPayout auTerms StandingBidState {..}
  | Just BidTerms {..} <- standingBidState =
      bt'BidPrice - totalAuctionFees auTerms
  | otherwise = 0
