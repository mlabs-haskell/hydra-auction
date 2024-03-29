module HydraAuction.Onchain.Types.Redeemers (
  AuctionMp'Redeemer (..),
  AuctionEscrow'Redeemer (..),
  AuctionMetadata'Redeemer (..),
  BidderDeposit'Redeemer (..),
  FeeEscrow'Redeemer (..),
  StandingBid'Redeemer (..),
  isConcluding,
) where

import PlutusTx.Prelude

import PlutusTx qualified

-- -------------------------------------------------------------------------
-- Auction state token minting policy
-- -------------------------------------------------------------------------
data AuctionMp'Redeemer
  = MintAuction
  | BurnAuction

instance Eq AuctionMp'Redeemer where
  MintAuction == MintAuction = True
  BurnAuction == BurnAuction = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''AuctionMp'Redeemer

-- -------------------------------------------------------------------------
-- Auction escrow validator
-- -------------------------------------------------------------------------
data AuctionEscrow'Redeemer
  = StartBidding
  | BidderBuys
  | SellerReclaims
  | CleanupAuction

instance Eq AuctionEscrow'Redeemer where
  StartBidding == StartBidding = True
  BidderBuys == BidderBuys = True
  SellerReclaims == SellerReclaims = True
  CleanupAuction == CleanupAuction = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''AuctionEscrow'Redeemer

isConcluding :: AuctionEscrow'Redeemer -> Bool
isConcluding BidderBuys = True
isConcluding SellerReclaims = True
isConcluding _ = False
--
{-# INLINEABLE isConcluding #-}

-- -------------------------------------------------------------------------
-- Auction metadata validator
-- -------------------------------------------------------------------------
data AuctionMetadata'Redeemer
  = RemoveAuction

instance Eq AuctionMetadata'Redeemer where
  _ == _ = True

PlutusTx.unstableMakeIsData ''AuctionMetadata'Redeemer

-- -------------------------------------------------------------------------
-- Bidder deposit validator
-- -------------------------------------------------------------------------
data BidderDeposit'Redeemer
  = DepositUsedToConcludeAuction
  | DepositReclaimedByLoser
  | DepositReclaimedAuctionConcluded
  | DepositCleanup

instance Eq BidderDeposit'Redeemer where
  DepositUsedToConcludeAuction == DepositUsedToConcludeAuction = True
  DepositReclaimedByLoser == DepositReclaimedByLoser = True
  DepositReclaimedAuctionConcluded == DepositReclaimedAuctionConcluded = True
  DepositCleanup == DepositCleanup = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''BidderDeposit'Redeemer

-- -------------------------------------------------------------------------
-- Fee escrow validator
-- -------------------------------------------------------------------------
data FeeEscrow'Redeemer
  = DistributeFees

instance Eq FeeEscrow'Redeemer where
  _ == _ = True

PlutusTx.unstableMakeIsData ''FeeEscrow'Redeemer

-- -------------------------------------------------------------------------
-- Standing bid validator
-- -------------------------------------------------------------------------
data StandingBid'Redeemer
  = NewBid
  | MoveToHydra
  | ConcludeAuction

instance Eq StandingBid'Redeemer where
  NewBid == NewBid = True
  MoveToHydra == MoveToHydra = True
  ConcludeAuction == ConcludeAuction = True
  _ == _ = True

PlutusTx.unstableMakeIsData ''StandingBid'Redeemer
