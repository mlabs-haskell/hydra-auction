-- | More Extras for Plutus.Extras
module HydraAuctionUtils.Extras.Plutus (
  module X,
  validatorAddress,
  scriptCurrencySymbol,
) where

-- Prelude imports

import HydraAuctionUtils.Prelude
import PlutusTx.Prelude (toBuiltin)

-- Plutus imports

import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V1.Address (Address, scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusTx qualified

-- Hydra imports

import Hydra.Cardano.Api (
  PlutusScriptVersion (..),
  SerialiseAsRawBytes (serialiseToRawBytes),
  fromPlutusScript,
  hashScript,
  pattern PlutusScript,
 )
import Hydra.Plutus.Extras as X

validatorAddress :: SerialisedScript -> Address
validatorAddress = scriptHashAddress . X.scriptValidatorHash PlutusScriptV2

{-# INLINEABLE scriptCurrencySymbol #-}
scriptCurrencySymbol :: SerialisedScript -> CurrencySymbol
scriptCurrencySymbol =
  CurrencySymbol
    . toBuiltin
    . serialiseToRawBytes
    . hashScript
    . PlutusScript
    . fromPlutusScript
