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
  CompiledCode (AuctionMetadata'ScriptHash -> TxOutRef -> MintingPolicyType)
auctionMpC =
  $$(PlutusTx.compile [||\am r -> detype (AuctionMp.mintingPolicy am r)||])
  where
    detype = detypeMintingPolicy @AuctionMp'Redeemer

-- -------------------------------------------------------------------------
-- Fee escrow validator
-- -------------------------------------------------------------------------
feeEscrowC ::
  CompiledCode (AuctionTerms -> ValidatorType)
feeEscrowC =
  $$(PlutusTx.compile [||detype . FeeEscrow.validator||])
  where
    detype = detypeStatelessValidator @FeeEscrow'Redeemer

-------------------------------------------------------------------------
-- Standing bid validator
-- -------------------------------------------------------------------------
standingBidC ::
  CompiledCode (AuctionId -> AuctionTerms -> ValidatorType)
standingBidC =
  $$(PlutusTx.compile [||\i a -> detype (StandingBid.validator i a)||])
  where
    detype = detypeValidator @StandingBidState @StandingBid'Redeemer

-- -------------------------------------------------------------------------
-- Auction escrow validator
-- -------------------------------------------------------------------------
auctionEscrowC ::
  CompiledCode
    ( StandingBid'ScriptHash ->
      FeeEscrow'ScriptHash ->
      AuctionId ->
      AuctionTerms ->
      ValidatorType
    )
auctionEscrowC =
  $$(PlutusTx.compile [||\s f i a -> detype (AuctionEscrow.validator s f i a)||])
  where
    detype = detypeValidator @AuctionEscrowState @AuctionEscrow'Redeemer

-- -------------------------------------------------------------------------
-- Bidder deposit validator
-- -------------------------------------------------------------------------
bidderDepositC ::
  CompiledCode
    ( AuctionEscrow'ScriptHash ->
      StandingBid'ScriptHash ->
      AuctionId ->
      AuctionTerms ->
      ValidatorType
    )
bidderDepositC =
  $$(PlutusTx.compile [||\s f i a -> detype (BidderDeposit.validator s f i a)||])
  where
    detype = detypeValidator @BidderInfo @BidderDeposit'Redeemer
