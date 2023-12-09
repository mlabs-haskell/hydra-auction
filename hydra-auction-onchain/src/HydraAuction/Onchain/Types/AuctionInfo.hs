module HydraAuction.Onchain.Types.AuctionInfo (
  AuctionInfo (..),
  auctionTN,
  auctionMetadataTN,
  standingBidTN,
  validateAuctionInfo,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Address (Address)
import PlutusLedgerApi.V1.Value (CurrencySymbol, TokenName)
import PlutusTx qualified

import HydraAuction.Error.Types.AuctionInfo (AuctionInfo'Error (..))
import HydraAuction.Onchain.Lib.Error (eCode, err)
import HydraAuction.Onchain.Types.AuctionTerms (
  AuctionTerms,
  validateAuctionTerms,
 )

data AuctionInfo = AuctionInfo
  { ai'AuctionId :: CurrencySymbol
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

-- -------------------------------------------------------------------------
-- Plutus instances
-- -------------------------------------------------------------------------
PlutusTx.unstableMakeIsData ''AuctionInfo

instance Eq AuctionInfo where
  (AuctionInfo x1 x2 x3 x4 x5 x6)
    == (AuctionInfo y1 y2 y3 y4 y5 y6) =
      x1 == y1
        && x2 == y2
        && x3 == y3
        && x4 == y4
        && x5 == y5
        && x6 == y6
