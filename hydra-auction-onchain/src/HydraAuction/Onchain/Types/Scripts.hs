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
  getBuyer,
  isConcluding,
  findAuctionEscrowOwnInput,
  findAuctionEscrowTokenInput,
  findAuctionEscrowInputAtSh,
  findAuctionEscrowTxOutAtSh,
  findAuctionEscrowTxOutAtAddr,
  --
  AuctionMetadata'Redeemer (..),
  AuctionMetadata'ScriptHash (..),
  findAuctionMetadataOwnInput,
  findAuctionMetadataTokenInput,
  findAuctionMetadataInputAtSh,
  findAuctionMetadataTxOutAtSh,
  findAuctionMetadataTxOutAtAddr,
  --
  BidderDeposit'Redeemer (..),
  --
  FeeEscrow'Redeemer (..),
  FeeEscrow'ScriptHash (..),
  valuePaidToFeeEscrow,
  --
  StandingBid'Redeemer (..),
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
  CurrencySymbol,
  PubKeyHash,
  ScriptContext,
  ScriptHash,
  TokenName,
  TxInInfo,
  TxInfo,
  TxOut (..),
  Value (..),
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

import HydraAuction.Onchain.Lib.PlutusTx (
  findInputWithStateToken,
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

instance Eq AuctionMP'Redeemer where
  MintAuction == MintAuction = True
  BurnAuction == BurnAuction = True
  _ == _ = False

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
-- Auction escrow validator
-- -------------------------------------------------------------------------
data AuctionEscrow'Redeemer
  = StartBidding
  | BidderBuys !PubKeyHash
  | SellerReclaims
  | CleanupAuction

instance Eq AuctionEscrow'Redeemer where
  StartBidding == StartBidding = True
  (BidderBuys x) == (BidderBuys y) = x == y
  SellerReclaims == SellerReclaims = True
  CleanupAuction == CleanupAuction = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''AuctionEscrow'Redeemer

newtype AuctionEscrow'ScriptHash = AuctionEscrow'ScriptHash
  { sh'AuctionEscrow :: ScriptHash
  }

getBuyer :: AuctionEscrow'Redeemer -> Maybe PubKeyHash
getBuyer (BidderBuys x) = Just x
getBuyer _ = Nothing
--
{-# INLINEABLE getBuyer #-}

isConcluding :: AuctionEscrow'Redeemer -> Bool
isConcluding (BidderBuys _) = True
isConcluding SellerReclaims = True
isConcluding _ = False
--
{-# INLINEABLE isConcluding #-}

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
data AuctionMetadata'Redeemer
  = RemoveAuction

instance Eq AuctionMetadata'Redeemer where
  _ == _ = True

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
data BidderDeposit'Redeemer
  = DepositUsedByWinner
  | DepositClaimedBySeller
  | DepositReclaimedByLoser
  | DepositReclaimedAuctionConcluded
  | DepositCleanup

instance Eq BidderDeposit'Redeemer where
  DepositUsedByWinner == DepositUsedByWinner = True
  DepositClaimedBySeller == DepositClaimedBySeller = True
  DepositReclaimedByLoser == DepositReclaimedByLoser = True
  DepositReclaimedAuctionConcluded == DepositReclaimedAuctionConcluded = True
  DepositCleanup == DepositCleanup = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''BidderDeposit'Redeemer

-- -------------------------------------------------------------------------
-- Fee escrow validator
-- -------------------------------------------------------------------------
data FeeEscrow'Redeemer
  = DistributeFees

instance Eq FeeEscrow'Redeemer where
  _ == _ = True

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

instance Eq StandingBid'Redeemer where
  NewBid == NewBid = True
  MoveToHydra == MoveToHydra = True
  ConcludeAuction == ConcludeAuction = True
  _ == _ = True

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
