module HydraAuction.Onchain.Scripts (
  AuctionEscrow'ScriptHash (..),
  auctionEscrowC,
  auctionEscrowS,
  auctionEscrowSh,
  --
  AuctionMetadata'ScriptHash (..),
  auctionMetadataC,
  auctionMetadataS,
  auctionMetadataSh,
  --
  AuctionMp'ScriptHash (..),
  auctionMpC,
  auctionMpS,
  auctionMpSh,
  --
  BidderDeposit'ScriptHash (..),
  bidderDepositC,
  bidderDepositS,
  bidderDepositSh,
  --
  FeeEscrow'ScriptHash (..),
  feeEscrowC,
  feeEscrowS,
  feeEscrowSh,
  --
  StandingBid'ScriptHash (..),
  standingBidC,
  standingBidS,
  standingBidSh,
) where

import PlutusTx.Prelude

import Cardano.Api.Shelley (PlutusScriptVersion (PlutusScriptV2))
import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.V2 (
  SerialisedScript,
  TxOutRef,
  serialiseCompiledCode,
 )
import PlutusTx (CompiledCode)
import PlutusTx qualified

import HydraAuction.Onchain.Lib.Scripts (
  MintingPolicyType,
  StatelessValidatorType,
  ValidatorType,
  scriptValidatorHash,
  wrapMintingPolicy,
  wrapStatelessValidator,
  wrapValidator,
 )
import HydraAuction.Onchain.Types.AuctionInfo (
  AuctionInfo,
 )
import HydraAuction.Onchain.Types.AuctionState (
  AuctionEscrowState,
  StandingBidState,
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
import HydraAuction.Onchain.Types.Tokens (
  AuctionId,
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
auctionMetadataC :: CompiledCode ValidatorType
auctionMetadataC =
  $$(PlutusTx.compile [||wrap AuctionMetadata.validator||])
  where
    wrap = wrapValidator @AuctionInfo @AuctionMetadata'Redeemer

auctionMetadataS :: SerialisedScript
auctionMetadataS = serialiseCompiledCode auctionMetadataC

auctionMetadataSh :: AuctionMetadata'ScriptHash
auctionMetadataSh =
  AuctionMetadata'ScriptHash $
    scriptValidatorHash PlutusScriptV2 auctionMetadataS

-- -------------------------------------------------------------------------
-- Auction token minting policy
-- -------------------------------------------------------------------------
auctionMpC :: CompiledCode (TxOutRef -> MintingPolicyType)
auctionMpC =
  $$(PlutusTx.compile [||\am r -> wrap (AuctionMp.mintingPolicy am r)||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 auctionMetadataSh
  where
    wrap = wrapMintingPolicy @AuctionMp'Redeemer

auctionMpS :: SerialisedScript
auctionMpS = serialiseCompiledCode auctionMpC

auctionMpSh :: AuctionMp'ScriptHash
auctionMpSh =
  AuctionMp'ScriptHash $
    scriptValidatorHash PlutusScriptV2 auctionMpS

-- -------------------------------------------------------------------------
-- Fee escrow validator
-- -------------------------------------------------------------------------
feeEscrowC :: CompiledCode (AuctionTerms -> StatelessValidatorType)
feeEscrowC =
  $$(PlutusTx.compile [||wrap . FeeEscrow.validator||])
  where
    wrap = wrapStatelessValidator @FeeEscrow'Redeemer

feeEscrowS :: SerialisedScript
feeEscrowS = serialiseCompiledCode feeEscrowC

feeEscrowSh :: FeeEscrow'ScriptHash
feeEscrowSh =
  FeeEscrow'ScriptHash $
    scriptValidatorHash PlutusScriptV2 feeEscrowS

-- -------------------------------------------------------------------------
-- Standing bid validator
-- -------------------------------------------------------------------------
standingBidC :: CompiledCode (AuctionId -> AuctionTerms -> ValidatorType)
standingBidC =
  $$(PlutusTx.compile [||\i a -> wrap (StandingBid.validator i a)||])
  where
    wrap = wrapValidator @StandingBidState @StandingBid'Redeemer

standingBidS :: SerialisedScript
standingBidS = serialiseCompiledCode standingBidC

standingBidSh :: StandingBid'ScriptHash
standingBidSh =
  StandingBid'ScriptHash $
    scriptValidatorHash PlutusScriptV2 standingBidS

-- -------------------------------------------------------------------------
-- Auction escrow validator
-- -------------------------------------------------------------------------
auctionEscrowC ::
  CompiledCode (AuctionId -> AuctionTerms -> ValidatorType)
auctionEscrowC =
  $$(PlutusTx.compile [||\s f i a -> wrap (AuctionEscrow.validator s f i a)||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 standingBidSh
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 feeEscrowSh
  where
    wrap = wrapValidator @AuctionEscrowState @AuctionEscrow'Redeemer

auctionEscrowS :: SerialisedScript
auctionEscrowS = serialiseCompiledCode auctionEscrowC

auctionEscrowSh :: AuctionEscrow'ScriptHash
auctionEscrowSh =
  AuctionEscrow'ScriptHash $
    scriptValidatorHash PlutusScriptV2 auctionEscrowS

-- -------------------------------------------------------------------------
-- Bidder deposit validator
-- -------------------------------------------------------------------------
bidderDepositC ::
  CompiledCode (AuctionId -> AuctionTerms -> ValidatorType)
bidderDepositC =
  $$(PlutusTx.compile [||\s f i a -> wrap (BidderDeposit.validator s f i a)||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 auctionEscrowSh
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 standingBidSh
  where
    wrap = wrapValidator @BidderInfo @BidderDeposit'Redeemer

bidderDepositS :: SerialisedScript
bidderDepositS = serialiseCompiledCode bidderDepositC

bidderDepositSh :: BidderDeposit'ScriptHash
bidderDepositSh =
  BidderDeposit'ScriptHash $
    scriptValidatorHash PlutusScriptV2 bidderDepositS
