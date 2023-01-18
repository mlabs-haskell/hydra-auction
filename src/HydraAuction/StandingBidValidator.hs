{-# LANGUAGE TemplateHaskell #-}

module HydraAuction.StandingBidValidator (
  mkStandingBidValidator,
  validatorScript,
  validatorHash,
) where

import Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import Plutus.V1.Ledger.Api (
  Script,
  ScriptContext,
  ValidatorHash,
  getValidator,
  mkValidatorScript,
 )
import PlutusTx qualified
import PlutusTx.Prelude

import HydraAuction.Types (StandingBidDatum)

{-# INLINE mkStandingBidValidator #-}
mkStandingBidValidator :: StandingBidDatum -> () -> ScriptContext -> Bool
mkStandingBidValidator _ _ _ = False

validatorScript :: Script
validatorScript = getValidator $ mkValidatorScript standingBidValidatorCompiled

validatorHash :: ValidatorHash
validatorHash = scriptValidatorHash validatorScript

standingBidValidatorCompiled :: PlutusTx.CompiledCode ValidatorType
standingBidValidatorCompiled = $$(PlutusTx.compile [||wrap mkStandingBidValidator||])
  where
    wrap = wrapValidator @StandingBidDatum @()
