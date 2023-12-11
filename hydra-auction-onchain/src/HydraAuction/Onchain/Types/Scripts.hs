module HydraAuction.Onchain.Types.Scripts (
  AuctionMP'ScriptHash (..),
  --
  AuctionEscrow'ScriptHash (..),
  findAuctionEscrowOwnInput,
  findAuctionEscrowTokenInput,
  findAuctionEscrowInputAtSh,
  findAuctionEscrowTxOutAtSh,
  findAuctionEscrowTxOutAtAddr,
  --
  AuctionMetadata'ScriptHash (..),
  findAuctionMetadataOwnInput,
  findAuctionMetadataTokenInput,
  findAuctionMetadataInputAtSh,
  findAuctionMetadataTxOutAtSh,
  findAuctionMetadataTxOutAtAddr,
  --
  BidderDeposit'ScriptHash (..),
  --
  FeeEscrow'ScriptHash (..),
  valuePaidToFeeEscrow,
  --
  StandingBid'ScriptHash (..),
  findStandingBidOwnInput,
  findStandingBidTokenInput,
  findStandingBidInputAtSh,
  findStandingBidTxOutAtSh,
  findStandingBidTxOutAtAddr,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V2 (
  Address,
  ScriptContext,
  ScriptHash,
  TxInInfo,
  TxInfo,
  TxOut (..),
  Value (..),
 )

import HydraAuction.Onchain.Lib.PlutusTx (
  findInputWithStateToken,
  findInputWithStateTokenAtSh,
  findOwnInputWithStateToken,
  findTxOutWithStateTokenAtAddr,
  findTxOutWithStateTokenAtSh,
  valuePaidToScript,
 )
import HydraAuction.Onchain.Types.Tokens (
  AuctionID (..),
  auctionMetadataTN,
  auctionTN,
  standingBidTN,
 )

-- -------------------------------------------------------------------------
-- Auction state token minting policy
-- -------------------------------------------------------------------------
newtype AuctionMP'ScriptHash = AuctionMP'ScriptHash
  { sh'AuctionMP :: ScriptHash
  }

-- -------------------------------------------------------------------------
-- Auction escrow validator
-- -------------------------------------------------------------------------
newtype AuctionEscrow'ScriptHash = AuctionEscrow'ScriptHash
  { sh'AuctionEscrow :: ScriptHash
  }

findAuctionEscrowOwnInput ::
  AuctionID ->
  ScriptContext ->
  Maybe TxInInfo
findAuctionEscrowOwnInput AuctionID {..} =
  findOwnInputWithStateToken auctionID auctionTN
--
{-# INLINEABLE findAuctionEscrowOwnInput #-}

findAuctionEscrowTokenInput ::
  AuctionID ->
  [TxInInfo] ->
  Maybe TxInInfo
findAuctionEscrowTokenInput AuctionID {..} =
  findInputWithStateToken
    auctionID
    auctionTN
--
{-# INLINEABLE findAuctionEscrowTokenInput #-}

findAuctionEscrowInputAtSh ::
  AuctionID ->
  AuctionEscrow'ScriptHash ->
  [TxInInfo] ->
  Maybe TxInInfo
findAuctionEscrowInputAtSh AuctionID {..} AuctionEscrow'ScriptHash {..} =
  findInputWithStateTokenAtSh
    auctionID
    auctionTN
    sh'AuctionEscrow
--
{-# INLINEABLE findAuctionEscrowInputAtSh #-}

findAuctionEscrowTxOutAtSh ::
  AuctionID ->
  AuctionEscrow'ScriptHash ->
  [TxOut] ->
  Maybe TxOut
findAuctionEscrowTxOutAtSh AuctionID {..} AuctionEscrow'ScriptHash {..} =
  findTxOutWithStateTokenAtSh
    auctionID
    auctionTN
    sh'AuctionEscrow
--
{-# INLINEABLE findAuctionEscrowTxOutAtSh #-}

findAuctionEscrowTxOutAtAddr ::
  AuctionID ->
  Address ->
  [TxOut] ->
  Maybe TxOut
findAuctionEscrowTxOutAtAddr AuctionID {..} =
  findTxOutWithStateTokenAtAddr
    auctionID
    auctionTN
--
{-# INLINEABLE findAuctionEscrowTxOutAtAddr #-}

-- -------------------------------------------------------------------------
-- Auction metadata validator
-- -------------------------------------------------------------------------
newtype AuctionMetadata'ScriptHash = AuctionMetadata'ScriptHash
  { sh'AuctionMetadata :: ScriptHash
  }

findAuctionMetadataOwnInput ::
  AuctionID ->
  ScriptContext ->
  Maybe TxInInfo
findAuctionMetadataOwnInput AuctionID {..} =
  findOwnInputWithStateToken auctionID auctionMetadataTN
--
{-# INLINEABLE findAuctionMetadataOwnInput #-}

findAuctionMetadataTokenInput ::
  AuctionID ->
  [TxInInfo] ->
  Maybe TxInInfo
findAuctionMetadataTokenInput AuctionID {..} =
  findInputWithStateToken
    auctionID
    auctionMetadataTN
--
{-# INLINEABLE findAuctionMetadataTokenInput #-}

findAuctionMetadataInputAtSh ::
  AuctionID ->
  AuctionMetadata'ScriptHash ->
  [TxInInfo] ->
  Maybe TxInInfo
findAuctionMetadataInputAtSh AuctionID {..} AuctionMetadata'ScriptHash {..} =
  findInputWithStateTokenAtSh
    auctionID
    auctionMetadataTN
    sh'AuctionMetadata
--
{-# INLINEABLE findAuctionMetadataInputAtSh #-}

findAuctionMetadataTxOutAtSh ::
  AuctionID ->
  AuctionMetadata'ScriptHash ->
  [TxOut] ->
  Maybe TxOut
findAuctionMetadataTxOutAtSh AuctionID {..} AuctionMetadata'ScriptHash {..} =
  findTxOutWithStateTokenAtSh
    auctionID
    auctionMetadataTN
    sh'AuctionMetadata
--
{-# INLINEABLE findAuctionMetadataTxOutAtSh #-}

findAuctionMetadataTxOutAtAddr ::
  AuctionID ->
  Address ->
  [TxOut] ->
  Maybe TxOut
findAuctionMetadataTxOutAtAddr AuctionID {..} =
  findTxOutWithStateTokenAtAddr
    auctionID
    auctionMetadataTN
--
{-# INLINEABLE findAuctionMetadataTxOutAtAddr #-}

-- -------------------------------------------------------------------------
-- Bidder deposit validator
-- -------------------------------------------------------------------------
newtype BidderDeposit'ScriptHash = BidderDeposit'ScriptHash
  { sh'BidderDeposit :: ScriptHash
  }

-- -------------------------------------------------------------------------
-- Fee escrow validator
-- -------------------------------------------------------------------------
newtype FeeEscrow'ScriptHash = FeeEscrow'ScriptHash
  { sh'FeeEscrow :: ScriptHash
  }

valuePaidToFeeEscrow :: TxInfo -> FeeEscrow'ScriptHash -> Value
valuePaidToFeeEscrow txInfo FeeEscrow'ScriptHash {..} =
  valuePaidToScript txInfo sh'FeeEscrow

-- -------------------------------------------------------------------------
-- Standing bid validator
-- -------------------------------------------------------------------------
newtype StandingBid'ScriptHash = StandingBid'ScriptHash
  { sh'StandingBid :: ScriptHash
  }

findStandingBidOwnInput ::
  AuctionID ->
  ScriptContext ->
  Maybe TxInInfo
findStandingBidOwnInput AuctionID {..} =
  findOwnInputWithStateToken auctionID standingBidTN
--
{-# INLINEABLE findStandingBidOwnInput #-}

findStandingBidTokenInput ::
  AuctionID ->
  [TxInInfo] ->
  Maybe TxInInfo
findStandingBidTokenInput AuctionID {..} =
  findInputWithStateToken
    auctionID
    standingBidTN
--
{-# INLINEABLE findStandingBidTokenInput #-}

findStandingBidInputAtSh ::
  AuctionID ->
  StandingBid'ScriptHash ->
  [TxInInfo] ->
  Maybe TxInInfo
findStandingBidInputAtSh AuctionID {..} StandingBid'ScriptHash {..} =
  findInputWithStateTokenAtSh
    auctionID
    standingBidTN
    sh'StandingBid
--
{-# INLINEABLE findStandingBidInputAtSh #-}

findStandingBidTxOutAtSh ::
  AuctionID ->
  StandingBid'ScriptHash ->
  [TxOut] ->
  Maybe TxOut
findStandingBidTxOutAtSh AuctionID {..} StandingBid'ScriptHash {..} =
  findTxOutWithStateTokenAtSh
    auctionID
    standingBidTN
    sh'StandingBid
--
{-# INLINEABLE findStandingBidTxOutAtSh #-}

findStandingBidTxOutAtAddr ::
  AuctionID ->
  Address ->
  [TxOut] ->
  Maybe TxOut
findStandingBidTxOutAtAddr AuctionID {..} =
  findTxOutWithStateTokenAtAddr
    auctionID
    standingBidTN
--
{-# INLINEABLE findStandingBidTxOutAtAddr #-}
