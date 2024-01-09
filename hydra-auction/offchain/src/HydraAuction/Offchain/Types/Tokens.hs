module HydraAuction.Offchain.Types.Tokens (
  AuctionMp (..),
  auctionMpToOnchain,
  allAuctionTokens,
  auctionTn,
  auctionMetadataTn,
  standingBidTn,
  mustMintAllAuctionTokens,
  mustBurnAllAuctionTokens,
) where

import Prelude

-- import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)

-- import Data.Set qualified as Set

import GeniusYield.TxBuilder (
  GYTxSkeleton,
  mustMint,
 )
import GeniusYield.Types (
  GYMintScript (..),
  GYMintingPolicy,
  GYTokenName,
  PlutusVersion (..),
  mintingPolicyCurrencySymbol,
  redeemerFromPlutusData,
  tokenNameFromPlutus,
 )

import HydraAuction.Onchain.Types.Redeemers qualified as O
import HydraAuction.Onchain.Types.Tokens qualified as O

-- -------------------------------------------------------------------------
-- Auction state token names
-- -------------------------------------------------------------------------

newtype AuctionMp = AuctionMp
  { auctionMp :: GYMintingPolicy 'PlutusV2
  }

auctionMpToOnchain :: AuctionMp -> O.AuctionId
auctionMpToOnchain AuctionMp {..} =
  O.AuctionId $ mintingPolicyCurrencySymbol auctionMp

-- -------------------------------------------------------------------------
-- Auction state token names
-- -------------------------------------------------------------------------
-- We assume that token name definitions are valid in the on-chain codebase.
-- For instance, they have valid length.

-- Auction state token, identifying the true auction escrow.
auctionTn :: GYTokenName
auctionTn = fromJust $ tokenNameFromPlutus O.auctionTn

-- Auction metadata token, identifying the true auction metadata.
auctionMetadataTn :: GYTokenName
auctionMetadataTn = fromJust $ tokenNameFromPlutus O.auctionMetadataTn

-- Standing bid token, identifying the true standing bid.
standingBidTn :: GYTokenName
standingBidTn = fromJust $ tokenNameFromPlutus O.standingBidTn

allAuctionTokens :: [GYTokenName]
allAuctionTokens = [auctionTn, auctionMetadataTn, standingBidTn]

-- -------------------------------------------------------------------------
-- Auction token minting/burning
-- -------------------------------------------------------------------------
mustMintAllAuctionTokens :: AuctionMp -> GYTxSkeleton 'PlutusV2
mustMintAllAuctionTokens AuctionMp {..} = foldMap mint allAuctionTokens
  where
    wit = GYMintScript auctionMp
    r = redeemerFromPlutusData O.MintAuction
    mint tn = mustMint wit r tn 1

mustBurnAllAuctionTokens :: AuctionMp -> GYTxSkeleton 'PlutusV2
mustBurnAllAuctionTokens AuctionMp {..} = foldMap burn allAuctionTokens
  where
    wit = GYMintScript auctionMp
    r = redeemerFromPlutusData O.BurnAuction
    burn tn = mustMint wit r tn (-1)
