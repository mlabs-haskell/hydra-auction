module HydraAuction.Onchain.Types.Scripts (
  AuctionMp'ScriptHash (..),
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
  AuctionId (..),
  auctionMetadataTn,
  auctionTn,
  standingBidTn,
 )

-- -------------------------------------------------------------------------
-- Auction state token minting policy
-- -------------------------------------------------------------------------
newtype AuctionMp'ScriptHash = AuctionMp'ScriptHash
  { sh'AuctionMp :: ScriptHash
  }

-- -------------------------------------------------------------------------
-- Auction escrow validator
-- -------------------------------------------------------------------------
newtype AuctionEscrow'ScriptHash = AuctionEscrow'ScriptHash
  { sh'AuctionEscrow :: ScriptHash
  }

findAuctionEscrowOwnInput ::
  AuctionId ->
  ScriptContext ->
  Maybe TxInInfo
findAuctionEscrowOwnInput AuctionId {..} =
  findOwnInputWithStateToken auctionId auctionTn
--
{-# INLINEABLE findAuctionEscrowOwnInput #-}

findAuctionEscrowTokenInput ::
  AuctionId ->
  [TxInInfo] ->
  Maybe TxInInfo
findAuctionEscrowTokenInput AuctionId {..} =
  findInputWithStateToken
    auctionId
    auctionTn
--
{-# INLINEABLE findAuctionEscrowTokenInput #-}

findAuctionEscrowInputAtSh ::
  AuctionId ->
  AuctionEscrow'ScriptHash ->
  [TxInInfo] ->
  Maybe TxInInfo
findAuctionEscrowInputAtSh AuctionId {..} AuctionEscrow'ScriptHash {..} =
  findInputWithStateTokenAtSh
    auctionId
    auctionTn
    sh'AuctionEscrow
--
{-# INLINEABLE findAuctionEscrowInputAtSh #-}

findAuctionEscrowTxOutAtSh ::
  AuctionId ->
  AuctionEscrow'ScriptHash ->
  [TxOut] ->
  Maybe TxOut
findAuctionEscrowTxOutAtSh AuctionId {..} AuctionEscrow'ScriptHash {..} =
  findTxOutWithStateTokenAtSh
    auctionId
    auctionTn
    sh'AuctionEscrow
--
{-# INLINEABLE findAuctionEscrowTxOutAtSh #-}

findAuctionEscrowTxOutAtAddr ::
  AuctionId ->
  Address ->
  [TxOut] ->
  Maybe TxOut
findAuctionEscrowTxOutAtAddr AuctionId {..} =
  findTxOutWithStateTokenAtAddr
    auctionId
    auctionTn
--
{-# INLINEABLE findAuctionEscrowTxOutAtAddr #-}

-- -------------------------------------------------------------------------
-- Auction metadata validator
-- -------------------------------------------------------------------------
newtype AuctionMetadata'ScriptHash = AuctionMetadata'ScriptHash
  { sh'AuctionMetadata :: ScriptHash
  }

findAuctionMetadataOwnInput ::
  AuctionId ->
  ScriptContext ->
  Maybe TxInInfo
findAuctionMetadataOwnInput AuctionId {..} =
  findOwnInputWithStateToken auctionId auctionMetadataTn
--
{-# INLINEABLE findAuctionMetadataOwnInput #-}

findAuctionMetadataTokenInput ::
  AuctionId ->
  [TxInInfo] ->
  Maybe TxInInfo
findAuctionMetadataTokenInput AuctionId {..} =
  findInputWithStateToken
    auctionId
    auctionMetadataTn
--
{-# INLINEABLE findAuctionMetadataTokenInput #-}

findAuctionMetadataInputAtSh ::
  AuctionId ->
  AuctionMetadata'ScriptHash ->
  [TxInInfo] ->
  Maybe TxInInfo
findAuctionMetadataInputAtSh AuctionId {..} AuctionMetadata'ScriptHash {..} =
  findInputWithStateTokenAtSh
    auctionId
    auctionMetadataTn
    sh'AuctionMetadata
--
{-# INLINEABLE findAuctionMetadataInputAtSh #-}

findAuctionMetadataTxOutAtSh ::
  AuctionId ->
  AuctionMetadata'ScriptHash ->
  [TxOut] ->
  Maybe TxOut
findAuctionMetadataTxOutAtSh AuctionId {..} AuctionMetadata'ScriptHash {..} =
  findTxOutWithStateTokenAtSh
    auctionId
    auctionMetadataTn
    sh'AuctionMetadata
--
{-# INLINEABLE findAuctionMetadataTxOutAtSh #-}

findAuctionMetadataTxOutAtAddr ::
  AuctionId ->
  Address ->
  [TxOut] ->
  Maybe TxOut
findAuctionMetadataTxOutAtAddr AuctionId {..} =
  findTxOutWithStateTokenAtAddr
    auctionId
    auctionMetadataTn
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
  AuctionId ->
  ScriptContext ->
  Maybe TxInInfo
findStandingBidOwnInput AuctionId {..} =
  findOwnInputWithStateToken auctionId standingBidTn
--
{-# INLINEABLE findStandingBidOwnInput #-}

findStandingBidTokenInput ::
  AuctionId ->
  [TxInInfo] ->
  Maybe TxInInfo
findStandingBidTokenInput AuctionId {..} =
  findInputWithStateToken
    auctionId
    standingBidTn
--
{-# INLINEABLE findStandingBidTokenInput #-}

findStandingBidInputAtSh ::
  AuctionId ->
  StandingBid'ScriptHash ->
  [TxInInfo] ->
  Maybe TxInInfo
findStandingBidInputAtSh AuctionId {..} StandingBid'ScriptHash {..} =
  findInputWithStateTokenAtSh
    auctionId
    standingBidTn
    sh'StandingBid
--
{-# INLINEABLE findStandingBidInputAtSh #-}

findStandingBidTxOutAtSh ::
  AuctionId ->
  StandingBid'ScriptHash ->
  [TxOut] ->
  Maybe TxOut
findStandingBidTxOutAtSh AuctionId {..} StandingBid'ScriptHash {..} =
  findTxOutWithStateTokenAtSh
    auctionId
    standingBidTn
    sh'StandingBid
--
{-# INLINEABLE findStandingBidTxOutAtSh #-}

findStandingBidTxOutAtAddr ::
  AuctionId ->
  Address ->
  [TxOut] ->
  Maybe TxOut
findStandingBidTxOutAtAddr AuctionId {..} =
  findTxOutWithStateTokenAtAddr
    auctionId
    standingBidTn
--
{-# INLINEABLE findStandingBidTxOutAtAddr #-}
