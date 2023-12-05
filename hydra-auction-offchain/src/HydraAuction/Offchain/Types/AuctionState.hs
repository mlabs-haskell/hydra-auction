module HydraAuction.Offchain.Types.AuctionState (
  AuctionEscrowState (..),
  StandingBidState (..),
  Buyer'Error (..),
  NewBid'Error (..),
  sellerPayout,
  validateBuyer,
  validateNewBid,
  fromPlutusAuctionEscrowState,
  fromPlutusStandingBidState,
  toPlutusAuctionEscrowState,
  toPlutusStandingBidState,
) where

import GHC.Generics (Generic)
import Prelude

import Data.Functor ((<&>))
import Data.Traversable (for)
import Data.Validation (Validation (..))

import Cardano.Api.Shelley (
  Lovelace (..),
  PolicyId (..),
 )

import HydraAuction.Error.Types.AuctionState (
  Buyer'Error (..),
  NewBid'Error (..),
 )
import HydraAuction.Offchain.Lib.Codec.Onchain (

 )
import HydraAuction.Offchain.Lib.Crypto (
  Hash,
  PaymentKey,
 )
import HydraAuction.Offchain.Lib.Validation (err, errWith)
import HydraAuction.Offchain.Types.AuctionTerms (
  AuctionTerms (..),
  totalAuctionFees,
 )
import HydraAuction.Offchain.Types.BidTerms (
  BidTerms (..),
  fromPlutusBidTerms,
  toPlutusBidTerms,
  validateBidTerms,
 )
import HydraAuction.Offchain.Types.BidderInfo (BidderInfo (..))

import HydraAuction.Onchain.Types.AuctionState qualified as O

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
      -- (NewBid01)
      -- The new bid state should not be empty.
      False
        `err` NewBid'Error'EmptyNewBid

validateNewBidTerms ::
  AuctionTerms ->
  PolicyId ->
  BidTerms ->
  Validation [NewBid'Error] ()
validateNewBidTerms auTerms auctionId newTerms =
  --
  -- (NewBid02)
  -- The new bid terms are valid.
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
  -- (NewBid03)
  -- The difference between the old and new bid price is
  -- no smaller than the auction's minimum bid increment.
  (bt'BidPrice oldTerms + at'MinBidIncrement <= bt'BidPrice newTerms)
    `err` NewBid'Error'InvalidBidIncrement

validateStartingBid ::
  AuctionTerms ->
  BidTerms ->
  Validation [NewBid'Error] ()
validateStartingBid AuctionTerms {..} BidTerms {..} =
  --
  -- (NewBid04)
  -- The first bid's price is
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
      -- (Buyer01)
      -- The buyer's hashed payment verification key corresponds
      -- to the bidder's payment verification key.
      (buyer == bi'BidderPkh)
        `err` Buyer'Error'BuyerVkPkhMismatch
        --
        -- (Buyer02)
        -- The bid terms are valid.
        <> validateBidTerms auTerms auctionId bidTerms
        `errWith` Buyer'Error'InvalidBidTerms
  | otherwise =
      --
      -- (Buyer03)
      -- Can only buy when standing bid state is non-empty.
      False
        `err` Buyer'Error'EmptyStandingBid

-- -------------------------------------------------------------------------
-- Seller payout
-- -------------------------------------------------------------------------

sellerPayout :: AuctionTerms -> StandingBidState -> Lovelace
sellerPayout auTerms StandingBidState {..}
  | Just BidTerms {..} <- standingBidState =
      bt'BidPrice - totalAuctionFees auTerms
  | otherwise = 0

-- -------------------------------------------------------------------------
-- Conversion to onchain
-- -------------------------------------------------------------------------
toPlutusAuctionEscrowState :: AuctionEscrowState -> O.AuctionEscrowState
toPlutusAuctionEscrowState AuctionAnnounced = O.AuctionAnnounced
toPlutusAuctionEscrowState BiddingStarted = O.BiddingStarted
toPlutusAuctionEscrowState AuctionConcluded = O.AuctionConcluded

toPlutusStandingBidState :: StandingBidState -> O.StandingBidState
toPlutusStandingBidState StandingBidState {..} =
  O.StandingBidState
    { O.standingBidState =
        standingBidState <&> toPlutusBidTerms
    }

-- -------------------------------------------------------------------------
-- Conversion from onchain
-- -------------------------------------------------------------------------
fromPlutusAuctionEscrowState :: O.AuctionEscrowState -> AuctionEscrowState
fromPlutusAuctionEscrowState O.AuctionAnnounced = AuctionAnnounced
fromPlutusAuctionEscrowState O.BiddingStarted = BiddingStarted
fromPlutusAuctionEscrowState O.AuctionConcluded = AuctionConcluded

fromPlutusStandingBidState :: O.StandingBidState -> Maybe StandingBidState
fromPlutusStandingBidState O.StandingBidState {..} = do
  m'standingBidState <-
    standingBidState `for` fromPlutusBidTerms
  pure $
    StandingBidState
      { standingBidState = m'standingBidState
      }
