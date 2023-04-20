module HydraAuction.Tx.Common (
  actorTipUtxo,
  addressAndKeys,
  minLovelace,
  mkInlineDatum,
  mkInlinedDatumScriptWitness,
  tokenToAsset,
  mintedTokens,
  scriptUtxos,
  scriptAddress,
  scriptPlutusScript,
  currentTimeSeconds,
  currentTimeMilliseconds,
  currentAuctionStage,
  toForgeStateToken,
  scriptSingleUtxo,
) where

-- Prelude imports
import Hydra.Prelude (ask)
import Prelude

-- Haskell imports
import Control.Monad (when)
import Control.Monad.TimeMachine (MonadTime (getCurrentTime))
import Data.Map qualified as Map
import Data.Time.Clock.POSIX qualified as POSIXTime
import Data.Tuple.Extra (first)

-- Plutus imports
import Plutus.V1.Ledger.Interval (member)
import Plutus.V1.Ledger.Value (
  TokenName (..),
 )
import Plutus.V2.Ledger.Api (
  POSIXTime (..),
  ToData,
  fromBuiltin,
  getMintingPolicy,
  getValidator,
  toBuiltinData,
  toData,
 )

-- Hydra imports

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (buildScriptAddress)
import Hydra.Cardano.Api (
  Address,
  AssetName,
  BuildTx,
  BuildTxWith,
  CtxUTxO,
  Lovelace (..),
  PaymentKey,
  PlutusScript,
  Quantity,
  ScriptDatum (..),
  ScriptWitness,
  ShelleyAddr,
  SigningKey,
  ToScriptData,
  TxIn,
  TxMintValue,
  TxOut,
  TxOutDatum,
  VerificationKey,
  WitCtxMint,
  WitCtxTxIn,
  Witness,
  fromPlutusData,
  fromPlutusScript,
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

-- Hydra auction imports

import HydraAuction.OnChain (
  AuctionScript (..),
  policy,
  scriptValidatorForTerms,
  singleUtxoScripts,
 )
import HydraAuction.OnChain.Common (stageToInterval)
import HydraAuction.OnChain.StateToken (
  StateTokenKind (..),
  stateTokenKindToTokenName,
 )
import HydraAuction.Runner (ExecutionContext (..), Runner)
import HydraAuction.Types (
  AuctionStage,
  AuctionTerms,
  VoucherForgingRedeemer (..),
  auctionStages,
 )
import HydraAuctionUtils.Monads (
  MonadNetworkId (..),
  MonadQueryUtxo (..),
  UtxoQuery (..),
  addressAndKeysForActor,
 )

minLovelace :: Lovelace
minLovelace = 2_000_000

currentTimeSeconds :: MonadTime timedMonad => timedMonad Integer
currentTimeSeconds =
  round . POSIXTime.utcTimeToPOSIXSeconds <$> getCurrentTime

currentTimeMilliseconds :: MonadTime timedMonad => timedMonad Integer
currentTimeMilliseconds =
  round . (* 1000) . POSIXTime.utcTimeToPOSIXSeconds <$> getCurrentTime

currentAuctionStage ::
  (MonadTime timedMonad) => AuctionTerms -> timedMonad AuctionStage
currentAuctionStage terms = do
  currentTime <- POSIXTime <$> currentTimeMilliseconds
  let matchingStages = filter (member currentTime . stageToInterval terms) auctionStages
  return $ case matchingStages of
    [stage] -> stage
    [_, stage] -> stage
    [] -> error "Impossible happend: no matching stages"
    _ -> error "Impossible happend: more than one matching stages"

tokenToAsset :: TokenName -> AssetName
tokenToAsset (TokenName t) = AssetName $ fromBuiltin t

toForgeStateToken :: AuctionTerms -> VoucherForgingRedeemer -> TxMintValue BuildTx
toForgeStateToken terms redeemer =
  mintedTokens
    (fromPlutusScript $ getMintingPolicy $ policy terms)
    redeemer
    [(tokenToAsset $ stateTokenKindToTokenName Voucher, num)]
  where
    num = case redeemer of
      MintVoucher -> 1
      BurnVoucher -> -1

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

addressAndKeys ::
  Runner
    ( Address ShelleyAddr
    , VerificationKey PaymentKey
    , SigningKey PaymentKey
    )
addressAndKeys = do
  MkExecutionContext {actor} <- ask
  addressAndKeysForActor actor

actorTipUtxo :: Runner UTxO.UTxO
actorTipUtxo = do
  (address, _, _) <- addressAndKeys
  queryUtxo (ByAddress address)

scriptPlutusScript :: AuctionScript -> AuctionTerms -> PlutusScript
scriptPlutusScript script terms = fromPlutusScript $ getValidator $ scriptValidatorForTerms script terms

scriptAddress :: MonadNetworkId m => AuctionScript -> AuctionTerms -> m (Address ShelleyAddr)
scriptAddress script terms =
  buildScriptAddress
    (PlutusScript $ scriptPlutusScript script terms)
    <$> askNetworkId

scriptUtxos :: (MonadNetworkId m, MonadQueryUtxo m) => AuctionScript -> AuctionTerms -> m UTxO.UTxO
scriptUtxos script terms = do
  scriptAddress' <- scriptAddress script terms
  queryUtxo (ByAddress scriptAddress')

scriptSingleUtxo ::
  (MonadNetworkId m, MonadQueryUtxo m, MonadFail m) =>
  AuctionScript ->
  AuctionTerms ->
  m (Maybe (TxIn, TxOut CtxUTxO))
scriptSingleUtxo script terms = do
  when (not $ script `elem` singleUtxoScripts) $
    fail $
      "Precondition failed: not single-utxo script: " <> show script
  utxos <- scriptUtxos script terms
  case UTxO.pairs utxos of
    [pair] -> return $ Just pair
    [] -> return Nothing
    _ ->
      fail $
        "Impossible happened: more than one UTxO for script " <> show script
