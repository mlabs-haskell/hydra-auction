{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Tx.Common (
  AutoCreateParams (..),
  filterAdaOnlyUtxo,
  actorTipUtxo,
  addressAndKeys,
  filterUtxoByCurrencySymbols,
  minLovelace,
  mkInlineDatum,
  mkInlinedDatumScriptWitness,
  autoSubmitAndAwaitTx,
  autoCreateTx,
  tokenToAsset,
  mintedTokens,
  scriptUtxos,
  scriptAddress,
  scriptPlutusScript,
  currentTimeSeconds,
  currentTimeMilliseconds,
  currentAuctionStage,
  toForgeStateToken,
) where

-- Prelude imports
import Hydra.Prelude (ask, toList)
import PlutusTx.Prelude (emptyByteString)
import Prelude

-- Haskell imports
import Control.Monad.TimeMachine (MonadTime (getCurrentTime))
import Data.List (sort)
import Data.Map qualified as Map
import Data.Time.Clock.POSIX qualified as POSIXTime
import Data.Tuple.Extra (first)

-- Plutus imports
import Plutus.V1.Ledger.Interval (member)
import Plutus.V1.Ledger.Value (
  CurrencySymbol (..),
  TokenName (..),
  symbols,
 )
import Plutus.V2.Ledger.Api (
  POSIXTime (..),
  ToData,
  fromBuiltin,
  getMintingPolicy,
  getValidator,
  toBuiltinData,
  toData,
  txOutValue,
 )

-- Hydra imports
import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (buildScriptAddress)
import Hydra.Cardano.Api (
  Address,
  AssetName,
  BuildTx,
  BuildTxWith,
  CtxTx,
  KeyWitness,
  Lovelace (..),
  PaymentKey,
  PlutusScript,
  Quantity,
  ScriptDatum (..),
  ScriptWitness,
  ShelleyAddr,
  SigningKey,
  ToScriptData,
  Tx,
  TxBody,
  TxBodyContent,
  TxBodyErrorAutoBalance,
  TxIn,
  TxMintValue,
  TxOut,
  TxOutDatum,
  UTxO,
  VerificationKey,
  WitCtxMint,
  WitCtxTxIn,
  Witness,
  balancedTxBody,
  fromPlutusData,
  fromPlutusScript,
  getVerificationKey,
  hashScript,
  makeShelleyKeyWitness,
  makeSignedTransaction,
  makeTransactionBodyAutoBalance,
  mkScriptWitness,
  scriptWitnessCtx,
  toPlutusTxOut,
  toScriptData,
  valueFromList,
  verificationKeyHash,
  withWitness,
  pattern AssetId,
  pattern AssetName,
  pattern BabbageEraInCardanoMode,
  pattern BuildTxWith,
  pattern PlutusScript,
  pattern PolicyId,
  pattern ScriptWitness,
  pattern ShelleyAddressInEra,
  pattern TxAuxScriptsNone,
  pattern TxBodyContent,
  pattern TxCertificatesNone,
  pattern TxExtraKeyWitnesses,
  pattern TxFeeExplicit,
  pattern TxInsCollateral,
  pattern TxInsReference,
  pattern TxMetadataNone,
  pattern TxMintValue,
  pattern TxOutDatumInline,
  pattern TxReturnCollateralNone,
  pattern TxScriptValidityNone,
  pattern TxTotalCollateralNone,
  pattern TxUpdateProposalNone,
  pattern TxValidityLowerBound,
  pattern TxValidityNoLowerBound,
  pattern TxValidityNoUpperBound,
  pattern TxValidityUpperBound,
  pattern TxWithdrawalsNone,
  pattern WitnessPaymentKey,
 )

-- Hydra auction imports

import HydraAuction.OnChain (AuctionScript (..), policy, scriptValidatorForTerms)
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
  BlockchainParams (..),
  MonadBlockchainParams (..),
  MonadNetworkId (..),
  MonadQueryUtxo (..),
  MonadSubmitTx,
  MonadTrace,
  UtxoQuery (..),
  addressAndKeysForActor,
  logMsg,
  submitAndAwaitTx,
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
    _ -> error "Impossible happend: more than one matching stage"

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

filterUtxoByCurrencySymbols :: [CurrencySymbol] -> UTxO -> UTxO
filterUtxoByCurrencySymbols symbolsToMatch = UTxO.filter hasExactlySymbols
  where
    hasExactlySymbols x =
      (sort . symbols . txOutValue <$> toPlutusTxOut x)
        == Just (sort symbolsToMatch)

filterAdaOnlyUtxo :: UTxO -> UTxO
filterAdaOnlyUtxo = filterUtxoByCurrencySymbols [CurrencySymbol emptyByteString]

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

data AutoCreateParams = AutoCreateParams
  { signedUtxos :: [(SigningKey PaymentKey, UTxO)]
  , additionalSigners :: [SigningKey PaymentKey]
  -- ^ List of keys that will sign the tx
  , referenceUtxo :: UTxO
  -- ^ Utxo which TxIns will be used as reference inputs
  , collateral :: Maybe TxIn
  -- ^ Nothing means collateral will be chosen automatically from given UTxOs
  , witnessedUtxos ::
      [(BuildTxWith BuildTx (Witness WitCtxTxIn), UTxO)]
  , outs :: [TxOut CtxTx]
  , toMint :: TxMintValue BuildTx
  , changeAddress :: Address ShelleyAddr
  , validityBound :: (Maybe POSIXTime, Maybe POSIXTime)
  }

autoCreateTx ::
  (MonadFail m, MonadBlockchainParams m) =>
  AutoCreateParams ->
  m Tx
autoCreateTx (AutoCreateParams {..}) = do
  let (lowerBound', upperBound') = validityBound
  lowerBound <- case lowerBound' of
    Nothing -> pure TxValidityNoLowerBound
    Just x -> TxValidityLowerBound <$> toSlotNo x
  upperBound <- case upperBound' of
    Nothing -> pure TxValidityNoUpperBound
    Just x -> TxValidityUpperBound <$> toSlotNo x

  MkBlockchainParams {protocolParameters} <-
    queryBlockchainParams
  body <-
    either (\x -> fail $ "Autobalance error: " <> show x) return
      =<< callBodyAutoBalance
        (allSignedUtxos <> allWitnessedUtxos <> referenceUtxo)
        (preBody protocolParameters lowerBound upperBound)
        changeAddress
  pure $ makeSignedTransaction (signingWitnesses body) body
  where
    allSignedUtxos = foldMap snd signedUtxos
    allWitnessedUtxos = foldMap snd witnessedUtxos
    allSKeys = additionalSigners <> (fst <$> signedUtxos)
    txInsToSign = toList (UTxO.inputSet allSignedUtxos)
    witnessedTxIns =
      [ (txIn, witness)
      | (witness, utxo) <- witnessedUtxos
      , txIn <- fst <$> UTxO.pairs utxo
      ]
    txInCollateral =
      case collateral of
        Just txIn -> txIn
        Nothing -> fst $ case UTxO.pairs $ filterAdaOnlyUtxo allSignedUtxos of
          x : _ -> x
          [] -> error "Cannot select collateral, cuz no money utxo was provided"
    preBody protocolParameters lowerBound upperBound =
      TxBodyContent
        ((withWitness <$> txInsToSign) <> witnessedTxIns)
        (TxInsCollateral [txInCollateral])
        (TxInsReference (toList $ UTxO.inputSet referenceUtxo))
        outs
        TxTotalCollateralNone
        TxReturnCollateralNone
        (TxFeeExplicit 0)
        (lowerBound, upperBound)
        TxMetadataNone
        TxAuxScriptsNone
        -- Adding all keys here, cuz other way `txSignedBy` does not see those
        -- signatures
        ( TxExtraKeyWitnesses $
            fmap (verificationKeyHash . getVerificationKey) allSKeys
        )
        (BuildTxWith $ Just protocolParameters)
        TxWithdrawalsNone
        TxCertificatesNone
        TxUpdateProposalNone
        toMint
        TxScriptValidityNone

    signingWitnesses :: TxBody -> [KeyWitness]
    signingWitnesses body = makeSignWitness <$> allSKeys
      where
        makeSignWitness sk = makeShelleyKeyWitness body (WitnessPaymentKey sk)
callBodyAutoBalance ::
  (MonadBlockchainParams m) =>
  UTxO ->
  TxBodyContent BuildTx ->
  Address ShelleyAddr ->
  m (Either TxBodyErrorAutoBalance TxBody)
callBodyAutoBalance
  utxo
  preBody
  changeAddress = do
    MkBlockchainParams {protocolParameters, systemStart, eraHistory, stakePools} <-
      queryBlockchainParams
    return $
      balancedTxBody
        <$> makeTransactionBodyAutoBalance
          BabbageEraInCardanoMode
          systemStart
          eraHistory
          protocolParameters
          stakePools
          (UTxO.toApi utxo)
          preBody
          (ShelleyAddressInEra changeAddress)
          Nothing

autoSubmitAndAwaitTx ::
  (MonadTrace m, MonadFail m, MonadBlockchainParams m, MonadSubmitTx m) =>
  AutoCreateParams ->
  m Tx
autoSubmitAndAwaitTx params = do
  tx <- autoCreateTx params
  logMsg "Signed"

  submitAndAwaitTx tx

  return tx
