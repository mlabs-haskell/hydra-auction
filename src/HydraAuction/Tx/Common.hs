{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Tx.Common (
  AutoCreateParams (..),
  filterAdaOnlyUtxo,
  fromPlutusAddressInRunner,
  actorTipUtxo,
  toSlotNo,
  addressAndKeys,
  networkIdToNetwork,
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
) where

-- Prelude imports
import Hydra.Prelude (ask, liftIO, toList)
import PlutusTx.Prelude (emptyByteString)
import Prelude

-- Haskell imports
import Control.Monad.TimeMachine (MonadTime (getCurrentTime))
import Data.List (sort)
import Data.Map qualified as Map
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock.POSIX qualified as POSIXTime
import Data.Tuple.Extra (first)

-- Cardano ledger imports
import Cardano.Ledger.BaseTypes qualified as Cardano

-- Plutus imports
import Plutus.V1.Ledger.Address qualified as PlutusAddress
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
  getValidator,
  toBuiltinData,
  toData,
  txOutValue,
 )

-- Hydra imports
import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (
  QueryPoint (QueryTip),
  buildAddress,
  buildScriptAddress,
  queryEraHistory,
  queryProtocolParameters,
  queryStakePools,
  querySystemStart,
 )
import CardanoNode (
  RunningNode (RunningNode, networkId, nodeSocket),
 )
import Hydra.Cardano.Api (
  Address,
  AddressInEra,
  AssetName,
  BuildTx,
  BuildTxWith,
  CtxTx,
  KeyWitness,
  Lovelace (..),
  NetworkId,
  PaymentKey,
  PlutusScript,
  Quantity,
  ScriptDatum (..),
  ScriptWitness,
  ShelleyAddr,
  SigningKey,
  SlotNo,
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
  fromPlutusAddress,
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
  pattern Mainnet,
  pattern PlutusScript,
  pattern PolicyId,
  pattern ScriptWitness,
  pattern ShelleyAddressInEra,
  pattern Testnet,
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
import Hydra.Chain.Direct.TimeHandle (queryTimeHandle, slotFromUTCTime)

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript, scriptValidatorForTerms)
import HydraAuction.OnChain.Common (stageToInterval)
import HydraAuction.Runner (ExecutionContext (..), Runner)
import HydraAuction.Types (AuctionStage, AuctionTerms, auctionStages)
import HydraAuctionUtils.Fixture (keysFor)
import HydraAuctionUtils.Monads (
  MonadNetworkId (..),
  MonadQueryUtxo (..),
  UtxoQuery (..),
  logMsg,
  submitAndAwaitTx,
 )

networkIdToNetwork :: NetworkId -> Cardano.Network
networkIdToNetwork (Testnet _) = Cardano.Testnet
networkIdToNetwork Mainnet = Cardano.Mainnet

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
  networkId' <- askNetworkId

  (actorVk, actorSk) <- liftIO $ keysFor actor
  let actorAddress = buildAddress actorVk networkId'

  logMsg $
    "Using actor: " <> show actor <> " with address: " <> show actorAddress

  pure (actorAddress, actorVk, actorSk)

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
  MkExecutionContext {actor} <- ask
  queryUtxo (ByActor actor)

fromPlutusAddressInRunner :: MonadNetworkId m => PlutusAddress.Address -> m AddressInEra
fromPlutusAddressInRunner address' = do
  networkId <- askNetworkId
  let network = networkIdToNetwork networkId
  return $
    fromPlutusAddress network address'

scriptPlutusScript :: AuctionScript -> AuctionTerms -> PlutusScript
scriptPlutusScript script terms = fromPlutusScript $ getValidator $ scriptValidatorForTerms script terms

scriptAddress :: MonadNetworkId m => AuctionScript -> AuctionTerms -> m (Address ShelleyAddr)
scriptAddress script terms =
  buildScriptAddress
    (PlutusScript $ scriptPlutusScript script terms)
    <$> askNetworkId

scriptUtxos :: AuctionScript -> AuctionTerms -> Runner UTxO.UTxO
scriptUtxos script terms = do
  scriptAddress' <- scriptAddress script terms
  queryUtxo (ByAddress scriptAddress')

data AutoCreateParams = AutoCreateParams
  { authoredUtxos :: [(SigningKey PaymentKey, UTxO)]
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

toSlotNo :: POSIXTime -> Runner SlotNo
toSlotNo ptime = do
  MkExecutionContext {node} <- ask
  timeHandle <-
    liftIO $
      queryTimeHandle (networkId node) (nodeSocket node)
  let timeInSeconds = getPOSIXTime ptime `div` 1000
      ndtime = secondsToNominalDiffTime $ fromInteger timeInSeconds
      utcTime = posixSecondsToUTCTime ndtime
  either (error . show) return $ slotFromUTCTime timeHandle utcTime

autoCreateTx :: AutoCreateParams -> Runner Tx
autoCreateTx (AutoCreateParams {..}) = do
  MkExecutionContext {node} <- ask
  let (lowerBound', upperBound') = validityBound
  lowerBound <- case lowerBound' of
    Nothing -> pure TxValidityNoLowerBound
    Just x -> TxValidityLowerBound <$> toSlotNo x
  upperBound <- case upperBound' of
    Nothing -> pure TxValidityNoUpperBound
    Just x -> TxValidityUpperBound <$> toSlotNo x

  liftIO $ do
    pparams <-
      queryProtocolParameters (networkId node) (nodeSocket node) QueryTip
    body <-
      either (\x -> error $ "Autobalance error: " <> show x) id
        <$> callBodyAutoBalance
          node
          (allAuthoredUtxos <> allWitnessedUtxos <> referenceUtxo)
          (preBody pparams lowerBound upperBound)
          changeAddress
    pure $ makeSignedTransaction (signingWitnesses body) body
  where
    allAuthoredUtxos = foldMap snd authoredUtxos
    allWitnessedUtxos = foldMap snd witnessedUtxos
    txInsToSign = toList (UTxO.inputSet allAuthoredUtxos)
    witnessedTxIns =
      [ (txIn, witness)
      | (witness, utxo) <- witnessedUtxos
      , txIn <- fst <$> UTxO.pairs utxo
      ]
    txInCollateral =
      case collateral of
        Just txIn -> txIn
        Nothing -> fst $ case UTxO.pairs $ filterAdaOnlyUtxo allAuthoredUtxos of
          x : _ -> x
          [] -> error "Cannot select collateral, cuz no money utxo was provided"
    preBody pparams lowerBound upperBound =
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
            fmap (verificationKeyHash . getVerificationKey . fst) authoredUtxos
        )
        (BuildTxWith $ Just pparams)
        TxWithdrawalsNone
        TxCertificatesNone
        TxUpdateProposalNone
        toMint
        TxScriptValidityNone
    makeSignWitness body sk = makeShelleyKeyWitness body (WitnessPaymentKey sk)
    signingWitnesses :: TxBody -> [KeyWitness]
    signingWitnesses body = fmap (makeSignWitness body . fst) authoredUtxos

callBodyAutoBalance ::
  RunningNode ->
  UTxO ->
  TxBodyContent BuildTx ->
  Address ShelleyAddr ->
  IO (Either TxBodyErrorAutoBalance TxBody)
callBodyAutoBalance
  (RunningNode {networkId, nodeSocket})
  utxo
  preBody
  changeAddress = do
    pparams <- queryProtocolParameters networkId nodeSocket QueryTip
    systemStart <- querySystemStart networkId nodeSocket QueryTip
    eraHistory <- queryEraHistory networkId nodeSocket QueryTip
    stakePools <- queryStakePools networkId nodeSocket QueryTip

    return $
      balancedTxBody
        <$> makeTransactionBodyAutoBalance
          BabbageEraInCardanoMode
          systemStart
          eraHistory
          pparams
          stakePools
          (UTxO.toApi utxo)
          preBody
          (ShelleyAddressInEra changeAddress)
          Nothing

autoSubmitAndAwaitTx :: AutoCreateParams -> Runner Tx
autoSubmitAndAwaitTx params = do
  tx <- autoCreateTx params
  logMsg "Signed"

  submitAndAwaitTx tx

  return tx
