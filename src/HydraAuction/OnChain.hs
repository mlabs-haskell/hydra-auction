module HydraAuction.OnChain (
  feeEscrowValidator,
  voucherAssetClass,
  scriptValidatorForTerms,
  AuctionScript (..),
  policy,
  standingBidValidator,
  escrowValidator,
  voucherCurrencySymbol,
  mkEscrowValidator,
  escrowAddress,
  standingBidAddress,
  singleUtxoScripts,
) where

-- Prelude imports

import GHC.Stack (HasCallStack)
import PlutusTx.Prelude
import Prelude qualified

-- Plutus imports
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1.Value (AssetClass (..), CurrencySymbol)
import PlutusTx qualified

-- Hydra auction imports
import HydraAuction.Addresses (
  DepositAddress (..),
  EscrowAddress (..),
  FeeEscrowAddress (..),
  StandingBidAddress (..),
 )
import HydraAuction.OnChain.Deposit (mkDepositValidator)
import HydraAuction.OnChain.Escrow (mkEscrowValidator)
import HydraAuction.OnChain.FeeEscrow (mkFeeEscrowValidator)
import HydraAuction.OnChain.StandingBid (mkStandingBidValidator)
import HydraAuction.OnChain.StateToken (
  StateTokenKind (..),
  mkPolicy,
  stateTokenKindToTokenName,
 )
import HydraAuction.Types (AuctionTerms (..))
import HydraAuctionUtils.Extras.Plutus (
  scriptCurrencySymbol,
  validatorAddress,
  wrapMintingPolicy,
  wrapValidator,
 )

-- Addresses

data AuctionScript = Escrow | StandingBid | FeeEscrow | Deposit
  deriving stock (Prelude.Show, Prelude.Eq)

-- This is scripts for which on-chain script ensure that
-- no more than single UTxO exists for given AuctionTerms
singleUtxoScripts :: [AuctionScript]
singleUtxoScripts = [Escrow, StandingBid, FeeEscrow]

scriptValidatorForTerms :: AuctionScript -> AuctionTerms -> SerialisedScript
scriptValidatorForTerms Escrow = escrowValidator
scriptValidatorForTerms StandingBid = standingBidValidator
scriptValidatorForTerms FeeEscrow = feeEscrowValidator
scriptValidatorForTerms Deposit = depositValidator

-- State Tokens

policy :: AuctionTerms -> SerialisedScript
policy terms =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode (escrowAddress terms, terms)

voucherCurrencySymbol :: AuctionTerms -> CurrencySymbol
voucherCurrencySymbol = scriptCurrencySymbol . policy

voucherAssetClass :: AuctionTerms -> AssetClass
voucherAssetClass terms = AssetClass (voucherCurrencySymbol terms, stateTokenKindToTokenName Voucher)

-- Escrow contract

{-# INLINEABLE escrowValidator #-}
escrowValidator :: AuctionTerms -> SerialisedScript
escrowValidator terms =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||wrapValidator . mkEscrowValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode (standingBidAddress terms, feeEscrowAddress terms, terms)

{-# INLINEABLE escrowAddress #-}
escrowAddress :: AuctionTerms -> EscrowAddress
escrowAddress = EscrowAddress . validatorAddress . escrowValidator

-- Standing bid contract

{-# INLINEABLE standingBidValidator #-}
standingBidValidator :: AuctionTerms -> SerialisedScript
standingBidValidator terms =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||wrapValidator . mkStandingBidValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode terms

{-# INLINEABLE standingBidAddress #-}
standingBidAddress :: AuctionTerms -> StandingBidAddress
standingBidAddress = StandingBidAddress . validatorAddress . standingBidValidator

-- Fee escrow

{-# INLINEABLE feeEscrowValidator #-}
feeEscrowValidator :: AuctionTerms -> SerialisedScript
feeEscrowValidator terms =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||wrapValidator . mkFeeEscrowValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode terms

{-# INLINEABLE feeEscrowAddress #-}
feeEscrowAddress :: AuctionTerms -> FeeEscrowAddress
feeEscrowAddress = FeeEscrowAddress . validatorAddress . feeEscrowValidator

-- Deposit

-- HACK: compilies incorrectly without HasCallStack
{-# INLINEABLE depositValidator #-}
depositValidator :: HasCallStack => AuctionTerms -> SerialisedScript
depositValidator terms =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||wrapValidator . mkDepositValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode (standingBidAddress terms, escrowAddress terms, terms)

{-# INLINEABLE depositAddress #-}
depositAddress :: AuctionTerms -> DepositAddress
depositAddress = DepositAddress . validatorAddress . depositValidator
