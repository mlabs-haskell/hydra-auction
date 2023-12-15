module Plutus.Cardano.Api.Codec.Script (
  -- Offchain -> Onchain
  toPlutusScriptHash,
  toPlutusScriptSerialisedV2,
  -- Onchain -> Offchain
  fromPlutusScriptHash,
  fromPlutusScriptSerializedV2,
  -- Offchain script -> Offchain script hash
  Cardano.Api.hashScript,
  Cardano.Api.scriptPolicyId,
  -- Onchain script -> Onchain script hash
  plutusScriptValidatorHashV2,
  plutusScriptCurrencySymbolV2,
) where

import Prelude

import Data.Function ((&))

import Cardano.Api.Shelley qualified as CA
import Cardano.Api.Shelley qualified as Cardano.Api

import PlutusLedgerApi.V2 qualified as PV2

import PlutusTx.Prelude qualified as Plutus

import Plutus.Cardano.Api.Codec.Value (toPlutusPolicyId)

-- -------------------------------------------------------------------------
-- Plutus script
-- -------------------------------------------------------------------------
toPlutusScriptSerialisedV2 ::
  CA.Script CA.PlutusScriptV2 ->
  PV2.SerialisedScript
toPlutusScriptSerialisedV2 cScript =
  let
    CA.PlutusScript CA.PlutusScriptV2 cScriptPlutus = cScript
    CA.PlutusScriptSerialised pScriptBytes = cScriptPlutus
   in
    pScriptBytes

fromPlutusScriptSerializedV2 ::
  PV2.SerialisedScript ->
  CA.Script CA.PlutusScriptV2
fromPlutusScriptSerializedV2 pScriptBytes =
  pScriptBytes
    & CA.PlutusScriptSerialised
    & CA.PlutusScript CA.PlutusScriptV2

-- -------------------------------------------------------------------------
-- Plutus script hash
-- -------------------------------------------------------------------------
toPlutusScriptHash ::
  CA.ScriptHash ->
  PV2.ScriptHash
toPlutusScriptHash cScriptHash =
  cScriptHash
    & CA.serialiseToRawBytes
    & PV2.toBuiltin
    & PV2.ScriptHash

fromPlutusScriptHash ::
  PV2.ScriptHash ->
  Maybe CA.ScriptHash
fromPlutusScriptHash (PV2.ScriptHash pScriptHashBytes) =
  pScriptHashBytes
    & Plutus.fromBuiltin
    & CA.deserialiseFromRawBytes CA.AsScriptHash
    & either (const Nothing) Just

-- -------------------------------------------------------------------------
-- Plutus script to Plutus (ScriptHash | CurrencySymbol) via Cardano API
-- -------------------------------------------------------------------------
plutusScriptValidatorHashV2 ::
  PV2.SerialisedScript ->
  PV2.ScriptHash
plutusScriptValidatorHashV2 pSerialisedScript =
  pSerialisedScript
    & fromPlutusScriptSerializedV2
    & CA.hashScript
    & toPlutusScriptHash

plutusScriptCurrencySymbolV2 ::
  PV2.SerialisedScript ->
  PV2.CurrencySymbol
plutusScriptCurrencySymbolV2 pSerialisedScript =
  pSerialisedScript
    & fromPlutusScriptSerializedV2
    & CA.scriptPolicyId
    & toPlutusPolicyId
