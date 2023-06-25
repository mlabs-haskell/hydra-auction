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
  getScriptByAddress,
) where

-- Prelude imports

import PlutusTx.Prelude
import Prelude qualified

-- Haskell imports
import Data.List qualified as List
import GHC.Stack (HasCallStack)

-- Plutus imports
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1.Address (Address)
import PlutusLedgerApi.V1.Value (AssetClass (..))
import PlutusTx qualified

-- Hydra auction imports
import HydraAuction.Addresses (
  DepositAddress (..),
  EscrowAddress (..),
  FeeEscrowAddress (..),
  StandingBidAddress (..),
  VoucherCS (..),
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
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Enum, Prelude.Bounded)

-- This is scripts for which on-chain script ensure that
-- no more than single UTxO exists for given AuctionTerms
singleUtxoScripts :: [AuctionScript]
singleUtxoScripts = [Escrow, StandingBid, FeeEscrow]

scriptValidatorForTerms :: AuctionScript -> AuctionTerms -> SerialisedScript
scriptValidatorForTerms Escrow = escrowValidator
scriptValidatorForTerms StandingBid = standingBidValidator
scriptValidatorForTerms FeeEscrow = feeEscrowValidator
scriptValidatorForTerms Deposit = depositValidator

enumInversion ::
  (Eq i, Prelude.Enum a, Prelude.Bounded a) =>
  (a -> i) ->
  i ->
  Maybe a
enumInversion func image =
  snd <$> List.find ((image ==) . fst) imageToDomain
  where
    domain = [Prelude.minBound .. Prelude.maxBound]
    imageToDomain = zip (map func domain) domain

getScriptByAddress :: AuctionTerms -> Address -> Maybe AuctionScript
getScriptByAddress terms = enumInversion addressByScript
  where
    addressByScript = validatorAddress . flip scriptValidatorForTerms terms

-- State Tokens

policy :: AuctionTerms -> SerialisedScript
policy terms =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode (escrowAddress terms, terms)

voucherCurrencySymbol :: AuctionTerms -> VoucherCS
voucherCurrencySymbol = VoucherCS . scriptCurrencySymbol . policy

voucherAssetClass :: AuctionTerms -> AssetClass
voucherAssetClass terms =
  AssetClass
    ( unVoucherCS $ voucherCurrencySymbol terms
    , stateTokenKindToTokenName Voucher
    )

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
