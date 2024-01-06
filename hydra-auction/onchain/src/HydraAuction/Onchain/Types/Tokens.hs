module HydraAuction.Onchain.Types.Tokens (
  AuctionId (..),
  auctionTn,
  auctionMetadataTn,
  standingBidTn,
  allAuctionTokensMinted,
  allAuctionTokensBurned,
  hasAuctionToken,
  hasAuctionMetadataToken,
  hasStandingBidToken,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V2 (
  CurrencySymbol,
  TokenName (..),
  TxOut (..),
  Value (..),
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

import HydraAuction.Onchain.Lib.PlutusTx (
  txOutHasStateToken,
 )

-- -------------------------------------------------------------------------
-- Auction state token names
-- -------------------------------------------------------------------------

newtype AuctionId = AuctionId
  { auctionCs :: CurrencySymbol
  }

PlutusTx.makeLift ''AuctionId

-- -------------------------------------------------------------------------
-- Auction state token names
-- -------------------------------------------------------------------------

-- Auction state token, identifying the true auction escrow.
auctionTn :: TokenName
auctionTn = TokenName "AUCTION"
--
{-# INLINEABLE auctionTn #-}

-- Auction metadata token, identifying the true auction metadata.
auctionMetadataTn :: TokenName
auctionMetadataTn = TokenName "AUCTION_METADATA"
--
{-# INLINEABLE auctionMetadataTn #-}

-- Standing bid token, identifying the true standing bid.
standingBidTn :: TokenName
standingBidTn = TokenName "STANDING_BID"
--
{-# INLINEABLE standingBidTn #-}

allAuctionTokensMinted :: AuctionId -> Value
allAuctionTokensMinted AuctionId {..} =
  Value $
    AssocMap.singleton auctionCs $
      AssocMap.fromList
        [ (auctionTn, 1)
        , (auctionMetadataTn, 1)
        , (standingBidTn, 1)
        ]
--
{-# INLINEABLE allAuctionTokensMinted #-}

allAuctionTokensBurned :: AuctionId -> Value
allAuctionTokensBurned AuctionId {..} =
  Value $
    AssocMap.singleton auctionCs $
      AssocMap.fromList
        [ (auctionTn, -1)
        , (auctionMetadataTn, -1)
        , (standingBidTn, -1)
        ]
--
{-# INLINEABLE allAuctionTokensBurned #-}

hasAuctionToken :: AuctionId -> TxOut -> Bool
hasAuctionToken AuctionId {..} =
  txOutHasStateToken auctionCs auctionTn
--
{-# INLINEABLE hasAuctionToken #-}

hasAuctionMetadataToken :: AuctionId -> TxOut -> Bool
hasAuctionMetadataToken AuctionId {..} =
  txOutHasStateToken auctionCs auctionMetadataTn
--
{-# INLINEABLE hasAuctionMetadataToken #-}

hasStandingBidToken :: AuctionId -> TxOut -> Bool
hasStandingBidToken AuctionId {..} =
  txOutHasStateToken auctionCs standingBidTn
--
{-# INLINEABLE hasStandingBidToken #-}
