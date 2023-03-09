module HydraAuction.Tx.Common.Utils (
  minLovelace,
  mintedTokens,
  tokenToAsset,
  filterAdaOnlyUtxo,
  filterUtxoByCurrencySymbols,
  currentTimeSeconds,
  networkIdToNetwork,
  scriptPlutusScript,
  mkInlineDatum,
  mkInlinedDatumScriptWitness,
) where

-- Prelude
import Hydra.Prelude (first, sort)
import Prelude

-- Haskell
import Data.Map qualified as Map

-- Cardano
import Cardano.Ledger.BaseTypes qualified as Cardano

-- Hydra
import Cardano.Api.UTxO
import Cardano.Api.UTxO qualified as UTxO
import Hydra.Cardano.Api (
  AssetName,
  BuildTx,
  BuildTxWith,
  Lovelace (..),
  NetworkId,
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
  fromPlutusScript,
  hashScript,
  mkScriptWitness,
  scriptWitnessCtx,
  toPlutusTxOut,
  toScriptData,
  valueFromList,
  pattern AssetId,
  pattern AssetName,
  pattern BuildTxWith,
  pattern Mainnet,
  pattern PlutusScript,
  pattern PolicyId,
  pattern ScriptWitness,
  pattern Testnet,
  pattern TxMintValue,
  pattern TxOutDatumInline,
 )

-- Plutus

import Data.Time.Clock.POSIX (getPOSIXTime)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api (
  ToData,
  getValidator,
  toBuiltinData,
  toData,
  txOutValue,
 )
import PlutusTx.Builtins

-- Hydra Auction
import HydraAuction.OnChain
import HydraAuction.Types

filterUtxoByCurrencySymbols :: [CurrencySymbol] -> UTxO -> UTxO
filterUtxoByCurrencySymbols symbolsToMatch = UTxO.filter hasExactlySymbols
  where
    hasExactlySymbols x =
      (sort . symbols . txOutValue <$> toPlutusTxOut x)
        == Just (sort symbolsToMatch)

filterAdaOnlyUtxo :: UTxO -> UTxO
filterAdaOnlyUtxo = filterUtxoByCurrencySymbols [CurrencySymbol emptyByteString]

scriptPlutusScript :: AuctionScript -> AuctionTerms -> PlutusScript
scriptPlutusScript script terms = fromPlutusScript $ getValidator $ scriptValidatorForTerms script terms

currentTimeSeconds :: IO Integer
currentTimeSeconds = round `fmap` getPOSIXTime

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

networkIdToNetwork :: NetworkId -> Cardano.Network
networkIdToNetwork (Testnet _) = Cardano.Testnet
networkIdToNetwork Mainnet = Cardano.Mainnet

minLovelace :: Lovelace
minLovelace = 2_000_000
