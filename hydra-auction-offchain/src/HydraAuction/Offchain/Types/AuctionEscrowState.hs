module HydraAuction.Offchain.Types.AuctionEscrowState (
  AuctionEscrowState (..),
  fromPlutusAuctionEscrowState,
  toPlutusAuctionEscrowState,
) where

import GHC.Generics (Generic)
import Prelude

import HydraAuction.Onchain.Types.AuctionEscrowState qualified as O

data AuctionEscrowState
  = AuctionAnnounced
  | BiddingStarted
  | AuctionConcluded
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Conversion to onchain
-- -------------------------------------------------------------------------
toPlutusAuctionEscrowState :: AuctionEscrowState -> O.AuctionEscrowState
toPlutusAuctionEscrowState AuctionAnnounced = O.AuctionAnnounced
toPlutusAuctionEscrowState BiddingStarted = O.BiddingStarted
toPlutusAuctionEscrowState AuctionConcluded = O.AuctionConcluded

-- -------------------------------------------------------------------------
-- Conversion from onchain
-- -------------------------------------------------------------------------
fromPlutusAuctionEscrowState :: O.AuctionEscrowState -> AuctionEscrowState
fromPlutusAuctionEscrowState O.AuctionAnnounced = AuctionAnnounced
fromPlutusAuctionEscrowState O.BiddingStarted = BiddingStarted
fromPlutusAuctionEscrowState O.AuctionConcluded = AuctionConcluded
