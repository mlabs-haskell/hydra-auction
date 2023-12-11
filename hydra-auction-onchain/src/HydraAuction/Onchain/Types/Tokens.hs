module HydraAuction.Onchain.Types.Tokens (
  AuctionID (..),
  auctionTN,
  auctionMetadataTN,
  standingBidTN,
  allAuctionTokensMinted,
  allAuctionTokensBurned,
  hasAuctionToken,
  hasAuctionMetadataToken,
  hasStandingBidToken,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V2 (
  CurrencySymbol,
  TokenName,
  TxOut (..),
  Value (..),
 )
import PlutusTx.AssocMap qualified as AssocMap

import HydraAuction.Onchain.Lib.PlutusTx (
  txOutHasStateToken,
 )

-- -------------------------------------------------------------------------
-- Auction state token names
-- -------------------------------------------------------------------------

newtype AuctionID = AuctionID
  { auctionID :: CurrencySymbol
  }

-- -------------------------------------------------------------------------
-- Auction state token names
-- -------------------------------------------------------------------------

-- Auction state token, identifying the true auction escrow.
auctionTN :: TokenName
auctionTN = "AUCTION"
--
{-# INLINEABLE auctionTN #-}

-- Auction metadata token, identifying the true auction metadata.
auctionMetadataTN :: TokenName
auctionMetadataTN = "AUCTION_METADATA"
--
{-# INLINEABLE auctionMetadataTN #-}

-- Standing bid token, identifying the true standing bid.
standingBidTN :: TokenName
standingBidTN = "STANDING_BID"
--
{-# INLINEABLE standingBidTN #-}

allAuctionTokensMinted :: AuctionID -> Value
allAuctionTokensMinted AuctionID {..} =
  Value $
    AssocMap.singleton auctionID $
      AssocMap.fromList
        [ (auctionTN, 1)
        , (auctionMetadataTN, 1)
        , (standingBidTN, 1)
        ]
--
{-# INLINEABLE allAuctionTokensMinted #-}

allAuctionTokensBurned :: AuctionID -> Value
allAuctionTokensBurned AuctionID {..} =
  Value $
    AssocMap.singleton auctionID $
      AssocMap.fromList
        [ (auctionTN, -1)
        , (auctionMetadataTN, -1)
        , (standingBidTN, -1)
        ]
--
{-# INLINEABLE allAuctionTokensBurned #-}

hasAuctionToken :: AuctionID -> TxOut -> Bool
hasAuctionToken AuctionID {..} =
  txOutHasStateToken auctionID auctionTN
--
{-# INLINEABLE hasAuctionToken #-}

hasAuctionMetadataToken :: AuctionID -> TxOut -> Bool
hasAuctionMetadataToken AuctionID {..} =
  txOutHasStateToken auctionID auctionMetadataTN
--
{-# INLINEABLE hasAuctionMetadataToken #-}

hasStandingBidToken :: AuctionID -> TxOut -> Bool
hasStandingBidToken AuctionID {..} =
  txOutHasStateToken auctionID standingBidTN
--
{-# INLINEABLE hasStandingBidToken #-}
