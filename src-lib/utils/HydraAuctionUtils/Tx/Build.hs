module HydraAuctionUtils.Tx.Build (
  minLovelace,
  tokenToAsset,
  mintedTokens,
  mkInlineDatum,
  mkInlinedDatumScriptWitness,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports
import Data.Map qualified as Map
import Data.Tuple.Extra (first)

-- Plutus imports
import PlutusLedgerApi.V1.Value (
  TokenName (..),
 )
import PlutusTx.Builtins.Class (fromBuiltin)
import PlutusTx.IsData.Class (
  ToData,
  toBuiltinData,
  toData,
 )

-- Hydra imports
import Hydra.Cardano.Api (
  AssetName,
  BuildTx,
  BuildTxWith,
  Lovelace (..),
  PlutusScript,
  Quantity,
  ScriptDatum (..),
  ScriptWitness,
  ToScriptData,
  TxMintValue,
  TxOutDatum,
  WitCtxMint,
  WitCtxTxIn,
  Witness,
  fromPlutusData,
  hashScript,
  mkScriptWitness,
  scriptWitnessInCtx,
  toScriptData,
  unsafeHashableScriptData,
  valueFromList,
  pattern AssetId,
  pattern AssetName,
  pattern BuildTxWith,
  pattern PlutusScript,
  pattern PolicyId,
  pattern ScriptWitness,
  pattern TxMintValue,
  pattern TxOutDatumInline,
 )

minLovelace :: Lovelace
minLovelace = 3_000_000

tokenToAsset :: TokenName -> AssetName
tokenToAsset (TokenName t) = AssetName $ fromBuiltin t

mintedTokens ::
  ToScriptData redeemer =>
  PlutusScript ->
  redeemer ->
  [(AssetName, Quantity)] ->
  TxMintValue BuildTx
mintedTokens script redeemer assets =
  TxMintValue mintedTokens' mintedWitnesses'
  where
    mintedTokens' = valueFromList (fmap (first (AssetId policyId)) assets)
    mintedWitnesses' =
      BuildTxWith $ Map.singleton policyId mintingWitness
    mintingWitness :: ScriptWitness WitCtxMint
    mintingWitness =
      mkScriptWitness script NoScriptDatumForMint (toScriptData redeemer)
    policyId =
      PolicyId $ hashScript $ PlutusScript script

mkInlineDatum :: ToScriptData datum => datum -> TxOutDatum ctx
mkInlineDatum x =
  TxOutDatumInline $
    unsafeHashableScriptData $
      fromPlutusData $
        toData $
          toBuiltinData x

mkInlinedDatumScriptWitness ::
  (ToData a) =>
  PlutusScript ->
  a ->
  BuildTxWith BuildTx (Witness WitCtxTxIn)
mkInlinedDatumScriptWitness script redeemer =
  BuildTxWith $
    ScriptWitness scriptWitnessInCtx $
      mkScriptWitness script InlineScriptDatum (toScriptData redeemer)
