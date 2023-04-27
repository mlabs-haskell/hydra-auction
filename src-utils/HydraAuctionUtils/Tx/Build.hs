module HydraAuctionUtils.Tx.Build (
  minLovelace,
  tokenToAsset,
  mintedTokens,
  mkInlineDatum,
  mkInlinedDatumScriptWitness,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Data.Map qualified as Map
import Data.Tuple.Extra (first)

-- Plutus imports
import Plutus.V1.Ledger.Value (
  TokenName (..),
 )
import Plutus.V2.Ledger.Api (
  ToData,
  fromBuiltin,
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
  scriptWitnessCtx,
  toScriptData,
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
minLovelace = 2_000_000

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
mkInlineDatum x = TxOutDatumInline $ fromPlutusData $ toData $ toBuiltinData x

mkInlinedDatumScriptWitness ::
  (ToData a) =>
  PlutusScript ->
  a ->
  BuildTxWith BuildTx (Witness WitCtxTxIn)
mkInlinedDatumScriptWitness script redeemer =
  BuildTxWith $
    ScriptWitness scriptWitnessCtx $
      mkScriptWitness script InlineScriptDatum (toScriptData redeemer)
