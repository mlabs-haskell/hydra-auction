module HydraAuctionOffchain.Contract.Types.AuctionState (
  AuctionEscrowState (..),
  StandingBidState (..),
  Buyer'Error (..),
  sellerPayout,
  validateBuyer,
  validateNewBid,
) where

import Prelude

import Data.Foldable (fold)
import Data.Validation (Validation (..))
import GHC.Generics (Generic)

import HydraAuctionOffchain.Lib.Validation (err, errWith)

import HydraAuctionOffchain.Contract.Types.AuctionTerms (
  AuctionTerms (..),
  totalAuctionFees,
 )
import HydraAuctionOffchain.Contract.Types.BidTerms (
  BidTerms (..),
  BidTerms'Error,
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

data NewBid'Error
  = NewBid'Error'EmptyNewBid
  | NewBid'Error'InvalidNewBidTerms BidTerms'Error
  | NewBid'Error'InvalidStartingBid
  | NewBid'Error'InvalidBidIncrement
  deriving stock (Eq, Generic, Show)

validateNewBid ::
  AuctionTerms ->
  PolicyId ->
  StandingBidState ->
  StandingBidState ->
  Validation [NewBid'Error] ()
validateNewBid auTerms auctionId oldBidState StandingBidState {..}
  | Just newTerms <- standingBidState =
      fold
        [ validateNewBidTerms auTerms auctionId newTerms
        , validateCompareBids auTerms oldBidState newTerms
        ]
  | otherwise =
      Failure [NewBid'Error'EmptyNewBid]

validateNewBidTerms ::
  AuctionTerms ->
  PolicyId ->
  BidTerms ->
  Validation [NewBid'Error] ()
validateNewBidTerms auTerms auctionId newTerms =
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

validateStartingBid ::
  AuctionTerms ->
  BidTerms ->
  Validation [NewBid'Error] ()
validateStartingBid AuctionTerms {..} BidTerms {..} =
  (at'StartingBid <= bt'BidPrice)
    `err` NewBid'Error'InvalidStartingBid

validateBidIncrement ::
  AuctionTerms ->
  BidTerms ->
  BidTerms ->
  Validation [NewBid'Error] ()
validateBidIncrement AuctionTerms {..} oldTerms newTerms =
  (bt'BidPrice oldTerms + at'MinBidIncrement <= bt'BidPrice newTerms)
    `err` NewBid'Error'InvalidBidIncrement

-- -------------------------------------------------------------------------
-- Buyer validation
-- -------------------------------------------------------------------------

data Buyer'Error
  = Buyer'Error'EmptyStandingBid
  | Buyer'Error'BuyerVkPkhMismatch
  | Buyer'Error'InvalidBidTerms BidTerms'Error
  deriving stock (Eq, Generic, Show)

validateBuyer ::
  AuctionTerms ->
  PolicyId ->
  StandingBidState ->
  Hash PaymentKey ->
  Validation [Buyer'Error] ()
validateBuyer auTerms auctionId StandingBidState {..} buyer
  | Just bidTerms@BidTerms {..} <- standingBidState
  , BidderInfo {..} <- bt'Bidder =
      fold
        [ (buyer == bi'BidderPkh)
            `err` Buyer'Error'BuyerVkPkhMismatch
        , validateBidTerms auTerms auctionId bidTerms
            `errWith` Buyer'Error'InvalidBidTerms
        ]
  | otherwise = Failure [Buyer'Error'EmptyStandingBid]

-- -------------------------------------------------------------------------
-- Seller payout
-- -------------------------------------------------------------------------

sellerPayout :: AuctionTerms -> StandingBidState -> Lovelace
sellerPayout auTerms StandingBidState {..}
  | Just BidTerms {..} <- standingBidState =
      bt'BidPrice - totalAuctionFees auTerms
  | otherwise = 0
