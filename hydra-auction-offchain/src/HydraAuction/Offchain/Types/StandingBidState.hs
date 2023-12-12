module HydraAuction.Offchain.Types.StandingBidState (
  StandingBidState (..),
  NewBid'Error (..),
  sellerPayout,
  validateNewBid,
  fromPlutusStandingBidState,
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

import HydraAuction.Error.Types.StandingBidState (
  NewBid'Error (..),
 )
import HydraAuction.Offchain.Lib.Codec.Onchain (

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

import HydraAuction.Onchain.Types.StandingBidState qualified as O

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
validateNewBid auTerms auctionCs oldBidState StandingBidState {..}
  | Just newTerms <- standingBidState =
      validateNewBidTerms auTerms auctionCs newTerms
        <> validateCompareBids auTerms oldBidState newTerms
  | otherwise =
      --
      -- The new bid state should not be empty.
      False
        `err` NewBid'Error'EmptyNewBid

validateNewBidTerms ::
  AuctionTerms ->
  PolicyId ->
  BidTerms ->
  Validation [NewBid'Error] ()
validateNewBidTerms auTerms auctionCs newTerms =
  --
  -- The new bid terms are valid.
  validateBidTerms auTerms auctionCs newTerms
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
  -- The first bid's price is
  -- no smaller than the auction's starting price.
  (at'StartingBid <= bt'BidPrice)
    `err` NewBid'Error'InvalidStartingBid

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
toPlutusStandingBidState :: StandingBidState -> O.StandingBidState
toPlutusStandingBidState StandingBidState {..} =
  O.StandingBidState
    { O.standingBidState =
        standingBidState <&> toPlutusBidTerms
    }

-- -------------------------------------------------------------------------
-- Conversion from onchain
-- -------------------------------------------------------------------------
fromPlutusStandingBidState :: O.StandingBidState -> Maybe StandingBidState
fromPlutusStandingBidState O.StandingBidState {..} = do
  m'standingBidState <-
    standingBidState `for` fromPlutusBidTerms
  pure $
    StandingBidState
      { standingBidState = m'standingBidState
      }
