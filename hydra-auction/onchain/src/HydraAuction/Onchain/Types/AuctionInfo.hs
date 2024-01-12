module HydraAuction.Onchain.Types.AuctionInfo (
  AuctionInfo (..),
  validateAuctionInfo,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V2 (
  Address,
  CurrencySymbol,
 )
import PlutusTx qualified

import HydraAuction.Error.Types.AuctionInfo (AuctionInfo'Error (..))
import HydraAuction.Onchain.Lib.Error (eCode, err)
import HydraAuction.Onchain.Types.AuctionTerms (
  AuctionTerms,
  validateAuctionTerms,
 )

-- -------------------------------------------------------------------------
-- Auction info -- published onchain for participants to discover auctions
-- -------------------------------------------------------------------------

data AuctionInfo = AuctionInfo
  { ai'AuctionCs :: CurrencySymbol
  -- ^ The auction is uniquely identified by
  -- the currency symbol of its state tokens.
  , ai'AuctionTerms :: AuctionTerms
  -- ^ The auction terms fully characterize the
  -- behaviour of the auction.
  , ai'AuctionEscrow :: Address
  , ai'BidderDeposit :: Address
  , ai'FeeEscrow :: Address
  , ai'StandingBid :: Address
  }

{- FOURMOLU_DISABLE -}
instance Eq AuctionInfo where
  ( AuctionInfo x1 x2 x3 x4 x5 x6) ==
   (AuctionInfo y1 y2 y3 y4 y5 y6) =
    x1 == y1 &&
    x2 == y2 &&
    x3 == y3 &&
    x4 == y4 &&
    x5 == y5 &&
    x6 == y6
{- FOURMOLU_ENABLE -}

PlutusTx.unstableMakeIsData ''AuctionInfo

-- -------------------------------------------------------------------------
-- Validation
-- -------------------------------------------------------------------------

validateAuctionInfo ::
  AuctionInfo ->
  Bool
validateAuctionInfo AuctionInfo {..} =
  --
  -- The auction terms in the metadata record should be valid.
  validateAuctionTerms ai'AuctionTerms
    `err` $(eCode $ AuctionInfo'Error'InvalidAuctionTerms [])
--
{-# INLINEABLE validateAuctionInfo #-}
