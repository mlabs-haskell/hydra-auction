module Scripts (
  mkAuctionId,
  mkAuctionInfo,
  mkAuctionScriptInfo,
  --
  AuctionEscrow'ScriptHash (..),
  auctionEscrowC,
  mkAuctionEscrowS,
  mkAuctionEscrowSh,
  --
  AuctionMetadata'ScriptHash (..),
  auctionMetadataC,
  auctionMetadataS,
  auctionMetadataSh,
  --
  AuctionMp'ScriptHash (..),
  auctionMpC,
  mkAuctionMpS,
  mkAuctionMpSh,
  --
  BidderDeposit'ScriptHash (..),
  bidderDepositC,
  mkBidderDepositS,
  mkBidderDepositSh,
  --
  FeeEscrow'ScriptHash (..),
  feeEscrowC,
  mkFeeEscrowS,
  mkFeeEscrowSh,
  --
  StandingBid'ScriptHash (..),
  standingBidC,
  mkStandingBidS,
  mkStandingBidSh,
) where

import PlutusTx.Prelude

import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  SerialisedScript,
  TxOutRef,
  serialiseCompiledCode,
 )
import PlutusTx (CompiledCode)
import PlutusTx qualified

import HydraAuction.Onchain.Types.AuctionEscrowState (
  AuctionEscrowState,
 )
import HydraAuction.Onchain.Types.AuctionInfo (
  AuctionInfo,
  AuctionScriptInfo (..),
  auctionScriptsToInfo,
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
import Lib.Scripts (
  MintingPolicyType,
  StatelessValidatorType,
  ValidatorType,
  wrapMintingPolicy,
  wrapStatelessValidator,
  wrapValidator,
 )
import Plutus.Cardano.Api.Codec (
  plutusScriptCurrencySymbolV2,
  plutusScriptValidatorHashV2,
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
  $$(PlutusTx.compile [||wrap AuctionMetadata.validator||])
  where
    wrap = wrapValidator @AuctionInfo @AuctionMetadata'Redeemer

auctionMetadataS :: SerialisedScript
auctionMetadataS = serialiseCompiledCode auctionMetadataC

auctionMetadataSh :: AuctionMetadata'ScriptHash
auctionMetadataSh =
  AuctionMetadata'ScriptHash $
    plutusScriptValidatorHashV2 auctionMetadataS

-- -------------------------------------------------------------------------
-- Auction token minting policy
-- -------------------------------------------------------------------------
auctionMpC ::
  CompiledCode (TxOutRef -> MintingPolicyType)
auctionMpC =
  $$(PlutusTx.compile [||\am r -> wrap (AuctionMp.mintingPolicy am r)||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 auctionMetadataSh
  where
    wrap = wrapMintingPolicy @AuctionMp'Redeemer

mkAuctionMpS :: TxOutRef -> SerialisedScript
mkAuctionMpS utxoNonce =
  serialiseCompiledCode $
    auctionMpC
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 utxoNonce

mkAuctionMpSh :: TxOutRef -> AuctionMp'ScriptHash
mkAuctionMpSh utxoNonce =
  AuctionMp'ScriptHash $
    plutusScriptValidatorHashV2 $
      mkAuctionMpS utxoNonce

mkAuctionCs :: TxOutRef -> CurrencySymbol
mkAuctionCs =
  plutusScriptCurrencySymbolV2
    . mkAuctionMpS

mkAuctionId :: TxOutRef -> AuctionId
mkAuctionId = AuctionId . mkAuctionCs

-- -------------------------------------------------------------------------
-- Auction info
-- -------------------------------------------------------------------------
mkAuctionInfo :: TxOutRef -> AuctionTerms -> AuctionInfo
mkAuctionInfo utxoNonce aTerms =
  auctionScriptsToInfo $ mkAuctionScriptInfo utxoNonce aTerms

mkAuctionScriptInfo :: TxOutRef -> AuctionTerms -> AuctionScriptInfo
mkAuctionScriptInfo utxoNonce aTerms =
  AuctionScriptInfo
    { as'AuctionId = auctionId
    , as'AuctionTerms = aTerms
    , as'AuctionEscrow = auctionEscrow
    , as'BidderDeposit = bidderDeposit
    , as'FeeEscrow = feeEscrow
    , as'StandingBid = standingBid
    }
  where
    auctionId = mkAuctionId utxoNonce
    feeEscrow = mkFeeEscrowSh aTerms
    standingBid = mkStandingBidSh auctionId aTerms
    auctionEscrow = mkAuctionEscrowSh standingBid feeEscrow auctionId aTerms
    bidderDeposit =
      mkBidderDepositSh auctionEscrow standingBid auctionId aTerms

-- -------------------------------------------------------------------------
-- Fee escrow validator
-- -------------------------------------------------------------------------
feeEscrowC ::
  CompiledCode (AuctionTerms -> StatelessValidatorType)
feeEscrowC =
  $$(PlutusTx.compile [||wrap . FeeEscrow.validator||])
  where
    wrap = wrapStatelessValidator @FeeEscrow'Redeemer

mkFeeEscrowS :: AuctionTerms -> SerialisedScript
mkFeeEscrowS aTerms =
  serialiseCompiledCode $
    feeEscrowC
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 aTerms

mkFeeEscrowSh :: AuctionTerms -> FeeEscrow'ScriptHash
mkFeeEscrowSh aTerms =
  FeeEscrow'ScriptHash $
    plutusScriptValidatorHashV2 $
      mkFeeEscrowS aTerms

-- -------------------------------------------------------------------------
-- Standing bid validator
-- -------------------------------------------------------------------------
standingBidC ::
  CompiledCode (AuctionId -> AuctionTerms -> ValidatorType)
standingBidC =
  $$(PlutusTx.compile [||\i a -> wrap (StandingBid.validator i a)||])
  where
    wrap = wrapValidator @StandingBidState @StandingBid'Redeemer

mkStandingBidS ::
  AuctionId -> AuctionTerms -> SerialisedScript
mkStandingBidS auctionId aTerms =
  serialiseCompiledCode $
    standingBidC
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 auctionId
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 aTerms

mkStandingBidSh :: AuctionId -> AuctionTerms -> StandingBid'ScriptHash
mkStandingBidSh auctionId aTerms =
  StandingBid'ScriptHash $
    plutusScriptValidatorHashV2 $
      mkStandingBidS auctionId aTerms

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
  $$(PlutusTx.compile [||\s f i a -> wrap (AuctionEscrow.validator s f i a)||])
  where
    wrap = wrapValidator @AuctionEscrowState @AuctionEscrow'Redeemer

mkAuctionEscrowS ::
  StandingBid'ScriptHash ->
  FeeEscrow'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  SerialisedScript
mkAuctionEscrowS sbsh fsh aid aTerms =
  serialiseCompiledCode $
    auctionEscrowC
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 sbsh
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 fsh
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 aid
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 aTerms

mkAuctionEscrowSh ::
  StandingBid'ScriptHash ->
  FeeEscrow'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  AuctionEscrow'ScriptHash
mkAuctionEscrowSh sbsh fsh aid aTerms =
  AuctionEscrow'ScriptHash $
    plutusScriptValidatorHashV2 $
      mkAuctionEscrowS sbsh fsh aid aTerms

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
  $$(PlutusTx.compile [||\s f i a -> wrap (BidderDeposit.validator s f i a)||])
  where
    wrap = wrapValidator @BidderInfo @BidderDeposit'Redeemer

mkBidderDepositS ::
  AuctionEscrow'ScriptHash ->
  StandingBid'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  SerialisedScript
mkBidderDepositS aesh sbsh aid aTerms =
  serialiseCompiledCode $
    bidderDepositC
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 aesh
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 sbsh
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 aid
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 aTerms

mkBidderDepositSh ::
  AuctionEscrow'ScriptHash ->
  StandingBid'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  BidderDeposit'ScriptHash
mkBidderDepositSh aesh sbsh aid aTerms =
  BidderDeposit'ScriptHash $
    plutusScriptValidatorHashV2 $
      mkBidderDepositS aesh sbsh aid aTerms
