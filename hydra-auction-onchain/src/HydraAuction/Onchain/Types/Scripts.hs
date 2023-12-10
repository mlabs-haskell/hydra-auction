module HydraAuction.Onchain.Types.Scripts (
  AuctionMP'Redeemer (..),
  AuctionID (..),
  AuctionMP'ScriptHash (..),
  --
  auctionTN,
  auctionMetadataTN,
  standingBidTN,
  allAuctionTokensMinted,
  allAuctionTokensBurned,
  hasAuctionToken,
  hasAuctionMetadataToken,
  hasStandingBidToken,
  --
  AuctionEscrow'Redeemer (..),
  AuctionEscrow'ScriptHash (..),
  findAuctionEscrowOwnInput,
  findAuctionEscrowInputAtSh,
  findAuctionEscrowTxOutAtSh,
  findAuctionEscrowTxOutAtAddr,
  --
  AuctionMetadata'Redeemer (..),
  AuctionMetadata'ScriptHash (..),
  findAuctionMetadataOwnInput,
  findAuctionMetadataInputAtSh,
  findAuctionMetadataTxOutAtSh,
  findAuctionMetadataTxOutAtAddr,
  --
  FeeEscrow'Redeemer (..),
  FeeEscrow'ScriptHash (..),
  valuePaidToFeeEscrow,
  --
  StandingBid'Redeemer (..),
  StandingBid'ScriptHash (..),
  findStandingBidOwnInput,
  findStandingBidInputAtSh,
  findStandingBidTxOutAtSh,
  findStandingBidTxOutAtAddr,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Address (
  Address,
 )
import PlutusLedgerApi.V1.Scripts (
  ScriptHash,
 )
import PlutusLedgerApi.V1.Value (
  CurrencySymbol,
  TokenName,
  Value (..),
 )
import PlutusLedgerApi.V2.Contexts (
  ScriptContext,
  TxInInfo,
  TxInfo,
 )
import PlutusLedgerApi.V2.Tx (
  TxOut (..),
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

import HydraAuction.Onchain.Lib.PlutusTx (
  findInputWithStateTokenAtSh,
  findOwnInputWithStateToken,
  findTxOutWithStateTokenAtAddr,
  findTxOutWithStateTokenAtSh,
  txOutHasStateToken,
  valuePaidToScript,
 )

-- -------------------------------------------------------------------------
-- Auction state token minting policy
-- -------------------------------------------------------------------------
data AuctionMP'Redeemer
  = MintAuction
  | BurnAuction

PlutusTx.unstableMakeIsData ''AuctionMP'Redeemer

newtype AuctionID = AuctionID
  { auctionID :: CurrencySymbol
  }

newtype AuctionMP'ScriptHash = AuctionMP'ScriptHash
  { sh'AuctionMP :: ScriptHash
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

-- Auction escrow validator
-- -------------------------------------------------------------------------
data AuctionEscrow'Redeemer
  = StartBidding
  | BidderBuys
  | SellerReclaims
  | CleanupAuction

PlutusTx.unstableMakeIsData ''AuctionEscrow'Redeemer

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
data AuctionMetadata'Redeemer
  = RemoveAuction

PlutusTx.unstableMakeIsData ''AuctionMetadata'Redeemer

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
-- Fee escrow validator
-- -------------------------------------------------------------------------
data FeeEscrow'Redeemer
  = DistributeFees

PlutusTx.unstableMakeIsData ''FeeEscrow'Redeemer

newtype FeeEscrow'ScriptHash = FeeEscrow'ScriptHash
  { sh'FeeEscrow :: ScriptHash
  }

valuePaidToFeeEscrow :: TxInfo -> FeeEscrow'ScriptHash -> Value
valuePaidToFeeEscrow txInfo FeeEscrow'ScriptHash {..} =
  valuePaidToScript txInfo sh'FeeEscrow

-- -------------------------------------------------------------------------
-- Standing bid validator
-- -------------------------------------------------------------------------
data StandingBid'Redeemer
  = NewBid
  | MoveToHydra
  | ConcludeAuction

PlutusTx.unstableMakeIsData ''StandingBid'Redeemer

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
