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
import PlutusTx qualified

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

PlutusTx.makeLift ''AuctionMp'ScriptHash

-- -------------------------------------------------------------------------
-- Auction escrow validator
-- -------------------------------------------------------------------------
newtype AuctionEscrow'ScriptHash = AuctionEscrow'ScriptHash
  { sh'AuctionEscrow :: ScriptHash
  }

PlutusTx.makeLift ''AuctionEscrow'ScriptHash

findAuctionEscrowOwnInput ::
  AuctionId ->
  ScriptContext ->
  Maybe TxInInfo
findAuctionEscrowOwnInput AuctionId {..} =
  findOwnInputWithStateToken auctionCs auctionTn
--
{-# INLINEABLE findAuctionEscrowOwnInput #-}

findAuctionEscrowTokenInput ::
  AuctionId ->
  [TxInInfo] ->
  Maybe TxInInfo
findAuctionEscrowTokenInput AuctionId {..} =
  findInputWithStateToken
    auctionCs
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
    auctionCs
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
    auctionCs
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
    auctionCs
    auctionTn
--
{-# INLINEABLE findAuctionEscrowTxOutAtAddr #-}

-- -------------------------------------------------------------------------
-- Auction metadata validator
-- -------------------------------------------------------------------------
newtype AuctionMetadata'ScriptHash = AuctionMetadata'ScriptHash
  { sh'AuctionMetadata :: ScriptHash
  }

PlutusTx.makeLift ''AuctionMetadata'ScriptHash

findAuctionMetadataOwnInput ::
  AuctionId ->
  ScriptContext ->
  Maybe TxInInfo
findAuctionMetadataOwnInput AuctionId {..} =
  findOwnInputWithStateToken auctionCs auctionMetadataTn
--
{-# INLINEABLE findAuctionMetadataOwnInput #-}

findAuctionMetadataTokenInput ::
  AuctionId ->
  [TxInInfo] ->
  Maybe TxInInfo
findAuctionMetadataTokenInput AuctionId {..} =
  findInputWithStateToken
    auctionCs
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
    auctionCs
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
    auctionCs
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
    auctionCs
    auctionMetadataTn
--
{-# INLINEABLE findAuctionMetadataTxOutAtAddr #-}

-- -------------------------------------------------------------------------
-- Bidder deposit validator
-- -------------------------------------------------------------------------
newtype BidderDeposit'ScriptHash = BidderDeposit'ScriptHash
  { sh'BidderDeposit :: ScriptHash
  }

PlutusTx.makeLift ''BidderDeposit'ScriptHash

-- -------------------------------------------------------------------------
-- Fee escrow validator
-- -------------------------------------------------------------------------
newtype FeeEscrow'ScriptHash = FeeEscrow'ScriptHash
  { sh'FeeEscrow :: ScriptHash
  }

PlutusTx.makeLift ''FeeEscrow'ScriptHash

valuePaidToFeeEscrow :: TxInfo -> FeeEscrow'ScriptHash -> Value
valuePaidToFeeEscrow txInfo FeeEscrow'ScriptHash {..} =
  valuePaidToScript txInfo sh'FeeEscrow

-- -------------------------------------------------------------------------
-- Standing bid validator
-- -------------------------------------------------------------------------
newtype StandingBid'ScriptHash = StandingBid'ScriptHash
  { sh'StandingBid :: ScriptHash
  }

PlutusTx.makeLift ''StandingBid'ScriptHash

findStandingBidOwnInput ::
  AuctionId ->
  ScriptContext ->
  Maybe TxInInfo
findStandingBidOwnInput AuctionId {..} =
  findOwnInputWithStateToken auctionCs standingBidTn
--
{-# INLINEABLE findStandingBidOwnInput #-}

findStandingBidTokenInput ::
  AuctionId ->
  [TxInInfo] ->
  Maybe TxInInfo
findStandingBidTokenInput AuctionId {..} =
  findInputWithStateToken
    auctionCs
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
    auctionCs
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
    auctionCs
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
    auctionCs
    standingBidTn
--
{-# INLINEABLE findStandingBidTxOutAtAddr #-}
