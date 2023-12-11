module HydraAuction.Onchain.Types.AuctionInfo (
  AuctionInfo (..),
  AuctionScriptInfo (..),
  auctionScriptsToInfo,
  validateAuctionInfo,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Address (scriptHashAddress)
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
import HydraAuction.Onchain.Types.Scripts (
  AuctionEscrow'ScriptHash (..),
  BidderDeposit'ScriptHash (..),
  FeeEscrow'ScriptHash (..),
  StandingBid'ScriptHash (..),
 )
import HydraAuction.Onchain.Types.Tokens (
  AuctionId (..),
 )

-- -------------------------------------------------------------------------
-- Auction info -- published onchain for participants to discover auctions
-- -------------------------------------------------------------------------

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

instance Eq AuctionInfo where
  (AuctionInfo x1 x2 x3 x4 x5 x6)
    == (AuctionInfo y1 y2 y3 y4 y5 y6) =
      x1 == y1
        && x2 == y2
        && x3 == y3
        && x4 == y4
        && x5 == y5
        && x6 == y6

PlutusTx.unstableMakeIsData ''AuctionInfo

-- -------------------------------------------------------------------------
-- Auction script info -- used internally to wire together scripts
-- -------------------------------------------------------------------------

data AuctionScriptInfo = AuctionScriptInfo
  { as'AuctionId :: AuctionId
  -- ^ The auction is uniquely identified by
  -- the currency symbol of its state tokens.
  , as'AuctionTerms :: AuctionTerms
  -- ^ The auction terms fully characterize the
  -- behaviour of the auction.
  , as'AuctionEscrow :: AuctionEscrow'ScriptHash
  , as'BidderDeposit :: BidderDeposit'ScriptHash
  , as'FeeEscrow :: FeeEscrow'ScriptHash
  , as'StandingBid :: StandingBid'ScriptHash
  }

auctionScriptsToInfo :: AuctionScriptInfo -> AuctionInfo
auctionScriptsToInfo AuctionScriptInfo {..} =
  AuctionInfo
    { ai'AuctionId = auctionId
    , ai'AuctionTerms = as'AuctionTerms
    , ai'AuctionEscrow = scriptHashAddress sh'AuctionEscrow
    , ai'BidderDeposit = scriptHashAddress sh'BidderDeposit
    , ai'FeeEscrow = scriptHashAddress sh'FeeEscrow
    , ai'StandingBid = scriptHashAddress sh'StandingBid
    }
  where
    AuctionId {..} = as'AuctionId
    AuctionEscrow'ScriptHash {..} = as'AuctionEscrow
    BidderDeposit'ScriptHash {..} = as'BidderDeposit
    FeeEscrow'ScriptHash {..} = as'FeeEscrow
    StandingBid'ScriptHash {..} = as'StandingBid

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
