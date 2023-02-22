module HydraAuction.OnChain (feeEscrowValidator, voucherAssetClass, scriptValidatorForTerms, AuctionScript (..), policy, standingBidValidator, escrowValidator, voucherCurrencySymbol, mkEscrowValidator, escrowAddress, standingBidAddress) where

-- Prelude imports

import PlutusTx.Prelude
import Prelude qualified

-- Plutus imports
import Plutus.V1.Ledger.Value (AssetClass (..))
import Plutus.V2.Ledger.Api (CurrencySymbol, MintingPolicy, ScriptContext, Validator, mkMintingPolicyScript, mkValidatorScript)
import PlutusTx qualified

-- Hydra auction imports
import HydraAuction.Addresses
import HydraAuction.OnChain.Escrow
import HydraAuction.OnChain.StandingBid
import HydraAuction.OnChain.StateToken
import HydraAuction.Plutus.Extras
import HydraAuction.Types

-- Addresses

data AuctionScript = Escrow | StandingBid | FeeEscrow
  deriving stock (Prelude.Show)

scriptValidatorForTerms :: AuctionScript -> AuctionTerms -> Validator
scriptValidatorForTerms Escrow = escrowValidator
scriptValidatorForTerms StandingBid = standingBidValidator
scriptValidatorForTerms FeeEscrow = feeEscrowValidator

-- State Tokens

policy :: AuctionTerms -> MintingPolicy
policy terms =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode (escrowAddress terms, terms)

voucherCurrencySymbol :: AuctionTerms -> CurrencySymbol
voucherCurrencySymbol = scriptCurrencySymbol . policy

voucherAssetClass :: AuctionTerms -> AssetClass
voucherAssetClass terms = AssetClass (voucherCurrencySymbol terms, stateTokenKindToTokenName Voucher)

-- Escrow contract

{-# INLINEABLE escrowValidator #-}
escrowValidator :: AuctionTerms -> Validator
escrowValidator terms =
  mkValidatorScript $
    $$(PlutusTx.compile [||wrapValidator . mkEscrowValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode (standingBidAddress terms, feeEscrowAddress terms, terms)

{-# INLINEABLE escrowAddress #-}
escrowAddress :: AuctionTerms -> EscrowAddress
escrowAddress = EscrowAddress . validatorAddress . escrowValidator

-- Standing bid contract

{-# INLINEABLE standingBidValidator #-}
standingBidValidator :: AuctionTerms -> Validator
standingBidValidator terms =
  mkValidatorScript $
    $$(PlutusTx.compile [||wrapValidator . mkStandingBidValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode terms

{-# INLINEABLE standingBidAddress #-}
standingBidAddress :: AuctionTerms -> StandingBidAddress
standingBidAddress = StandingBidAddress . validatorAddress . standingBidValidator

-- Fee escrow

mkFeeEscrowValidator :: AuctionTerms -> () -> () -> ScriptContext -> Bool
mkFeeEscrowValidator _terms _datum () _context = True -- FIXUP: Implement

{-# INLINEABLE feeEscrowValidator #-}
feeEscrowValidator :: AuctionTerms -> Validator
feeEscrowValidator terms =
  mkValidatorScript $
    $$(PlutusTx.compile [||wrapValidator . mkFeeEscrowValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode terms

{-# INLINEABLE feeEscrowAddress #-}
feeEscrowAddress :: AuctionTerms -> FeeEscrowAddress
feeEscrowAddress = FeeEscrowAddress . validatorAddress . feeEscrowValidator
