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
  AuctionMetadata'Redeemer (..),
  AuctionMetadata'ScriptHash (..),
  findAuctionMetadataOwnInput,
  findAuctionMetadataTxOutAtSh,
  findAuctionMetadataTxOutAtAddr,
  --
  FeeEscrow'Redeemer (..),
  FeeEscrow'ScriptHash (..),
  --
  StandingBid'Redeemer (..),
  StandingBid'ScriptHash (..),
  findStandingBidOwnInput,
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
 )
import PlutusLedgerApi.V2.Tx (
  TxOut (..),
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

import HydraAuction.Onchain.Lib.PlutusTx (
  findOwnInputWithStateToken,
  findTxOutWithStateTokenAtAddr,
  findTxOutWithStateTokenAtSh,
  txOutHasStateToken,
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
