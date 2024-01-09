module HydraAuction.Onchain.Compiled (
  AuctionEscrow'ScriptHash (..),
  auctionEscrowC,
  --
  AuctionMetadata'ScriptHash (..),
  auctionMetadataC,
  --
  AuctionMp'ScriptHash (..),
  auctionMpC,
  --
  BidderDeposit'ScriptHash (..),
  bidderDepositC,
  --
  FeeEscrow'ScriptHash (..),
  feeEscrowC,
  --
  StandingBid'ScriptHash (..),
  standingBidC,
) where

import PlutusTx.Prelude

import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.V2 (
  TxOutRef,
 )
import PlutusTx (CompiledCode)
import PlutusTx qualified

import HydraAuction.Onchain.Lib.Compile (
  MintingPolicyType,
  ValidatorType,
  detypeMintingPolicy,
  detypeStatelessValidator,
  detypeValidator,
 )
import HydraAuction.Onchain.Types.AuctionEscrowState (
  AuctionEscrowState,
 )
import HydraAuction.Onchain.Types.AuctionInfo (
  AuctionInfo,
 )
import HydraAuction.Onchain.Types.AuctionTerms (
  AuctionTerms,
 )
import HydraAuction.Onchain.Types.BidderInfo (
  BidderInfo,
 )
import HydraAuction.Onchain.Types.Redeemers (
  AuctionEscrow'Redeemer,
  AuctionMetadata'Redeemer,
  AuctionMp'Redeemer,
  BidderDeposit'Redeemer,
  FeeEscrow'Redeemer,
  StandingBid'Redeemer,
 )
import HydraAuction.Onchain.Types.Scripts (
  AuctionEscrow'ScriptHash (..),
  AuctionMetadata'ScriptHash (..),
  AuctionMp'ScriptHash (..),
  BidderDeposit'ScriptHash (..),
  FeeEscrow'ScriptHash (..),
  StandingBid'ScriptHash (..),
 )
import HydraAuction.Onchain.Types.StandingBidState (
  StandingBidState,
 )
import HydraAuction.Onchain.Types.Tokens (
  AuctionId (..),
 )

import HydraAuction.Onchain.MintingPolicies.AuctionMp qualified as AuctionMp
import HydraAuction.Onchain.Validators.AuctionEscrow qualified as AuctionEscrow
import HydraAuction.Onchain.Validators.AuctionMetadata qualified as AuctionMetadata
import HydraAuction.Onchain.Validators.BidderDeposit qualified as BidderDeposit
import HydraAuction.Onchain.Validators.FeeEscrow qualified as FeeEscrow
import HydraAuction.Onchain.Validators.StandingBid qualified as StandingBid

-- -------------------------------------------------------------------------
-- Auction metadata validator
-- -------------------------------------------------------------------------
auctionMetadataC ::
  CompiledCode ValidatorType
auctionMetadataC =
  $$(PlutusTx.compile [||detype AuctionMetadata.validator||])
  where
    detype = detypeValidator @AuctionInfo @AuctionMetadata'Redeemer

-- -------------------------------------------------------------------------
-- Auction token minting policy
-- -------------------------------------------------------------------------
auctionMpC ::
  AuctionMetadata'ScriptHash ->
  TxOutRef ->
  CompiledCode MintingPolicyType
auctionMpC amsh utxoNonce =
  $$(PlutusTx.compile [||\am r -> detype (AuctionMp.mintingPolicy am r)||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 amsh
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 utxoNonce
  where
    detype = detypeMintingPolicy @AuctionMp'Redeemer

-- -------------------------------------------------------------------------
-- Fee escrow validator
-- -------------------------------------------------------------------------
feeEscrowC ::
  AuctionTerms ->
  CompiledCode ValidatorType
feeEscrowC aTerms =
  $$(PlutusTx.compile [||detype . FeeEscrow.validator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 aTerms
  where
    detype = detypeStatelessValidator @FeeEscrow'Redeemer

-------------------------------------------------------------------------
-- Standing bid validator
-- -------------------------------------------------------------------------
standingBidC ::
  AuctionId ->
  AuctionTerms ->
  CompiledCode ValidatorType
standingBidC auctionId aTerms =
  $$(PlutusTx.compile [||\i a -> detype (StandingBid.validator i a)||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 auctionId
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 aTerms
  where
    detype = detypeValidator @StandingBidState @StandingBid'Redeemer

-- -------------------------------------------------------------------------
-- Auction escrow validator
-- -------------------------------------------------------------------------
auctionEscrowC ::
  StandingBid'ScriptHash ->
  FeeEscrow'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  CompiledCode ValidatorType
auctionEscrowC sbsh fsh auctionId aTerms =
  $$(PlutusTx.compile [||\s f i a -> detype (AuctionEscrow.validator s f i a)||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 sbsh
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 fsh
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 auctionId
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 aTerms
  where
    detype = detypeValidator @AuctionEscrowState @AuctionEscrow'Redeemer

-- -------------------------------------------------------------------------
-- Bidder deposit validator
-- -------------------------------------------------------------------------
bidderDepositC ::
  AuctionEscrow'ScriptHash ->
  StandingBid'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  CompiledCode ValidatorType
bidderDepositC aesh sbsh auctionId aTerms =
  $$(PlutusTx.compile [||\s f i a -> detype (BidderDeposit.validator s f i a)||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 aesh
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 sbsh
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 auctionId
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 aTerms
  where
    detype = detypeValidator @BidderInfo @BidderDeposit'Redeemer
