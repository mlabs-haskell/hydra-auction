{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Tx.Common.Runner (
  autoSubmitAndAwaitTx,
  actorTipUtxo,
  addressAndKeys,
  fromPlutusAddressInRunner,
  queryUTxOByTxInInRunner,
  scriptAddress,
  scriptUtxos,
  toSlotNo,
) where

-- Prelude
import Hydra.Prelude (ask, liftIO, void)
import Prelude

-- Haskell
import Data.Set (toList)
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- Cardano
import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (
  QueryPoint (QueryTip),
  awaitTransaction,
  buildAddress,
  buildScriptAddress,
  queryEraHistory,
  queryProtocolParameters,
  queryStakePools,
  querySystemStart,
  queryUTxO,
  queryUTxOByTxIn,
  queryUTxOFor,
  submitTransaction,
 )
import CardanoNode (RunningNode (RunningNode, nodeSocket), networkId)

-- Plutus
import Plutus.V1.Ledger.Address qualified as PlutusAddress
import Plutus.V1.Ledger.Api (POSIXTime (getPOSIXTime))

-- Hydra
import Hydra.Cardano.Api (
  Address,
  AddressInEra,
  BuildTx,
  KeyWitness,
  PaymentKey,
  ShelleyAddr,
  SigningKey,
  SlotNo,
  Tx,
  TxBody,
  TxBodyContent,
  TxBodyErrorAutoBalance,
  TxIn,
  UTxO,
  VerificationKey,
  balancedTxBody,
  fromPlutusAddress,
  getTxId,
  getVerificationKey,
  makeShelleyKeyWitness,
  makeSignedTransaction,
  makeTransactionBodyAutoBalance,
  txBody,
  verificationKeyHash,
  withWitness,
  pattern BabbageEraInCardanoMode,
  pattern BuildTxWith,
  pattern PlutusScript,
  pattern ShelleyAddressInEra,
  pattern TxAuxScriptsNone,
  pattern TxBodyContent,
  pattern TxCertificatesNone,
  pattern TxExtraKeyWitnesses,
  pattern TxFeeExplicit,
  pattern TxInsCollateral,
  pattern TxInsReference,
  pattern TxMetadataNone,
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
import Hydra.Chain.Direct.TimeHandle (TimeHandle (..), queryTimeHandle)

-- Hydra Auction
import HydraAuction.Fixture
import HydraAuction.OnChain (AuctionScript)
import HydraAuction.Runner (ExecutionContext (..), Runner, logMsg)
import HydraAuction.Tx.Common.Types (
  AutoCreateParams (AutoCreateParams, outs, toMint),
  authoredUtxos,
  changeAddress,
  collateral,
  referenceUtxo,
  validityBound,
  witnessedUtxos,
 )
import HydraAuction.Tx.Common.Utils
import HydraAuction.Types

autoSubmitAndAwaitTx :: AutoCreateParams -> Runner Tx
autoSubmitAndAwaitTx params = do
  MkExecutionContext {node} <- ask
  let networkId' = networkId node
      nodeSocket' = nodeSocket node

  tx <- autoCreateTx params
  logMsg "Signed"

  liftIO $
    submitTransaction
      networkId'
      nodeSocket'
      tx

  logMsg "Submited"

  void $
    liftIO $
      awaitTransaction
        networkId'
        nodeSocket'
        tx

  logMsg $ "Created Tx id: " <> show (getTxId $ txBody tx)
  pure tx

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

scriptUtxos :: AuctionScript -> AuctionTerms -> Runner UTxO.UTxO
scriptUtxos script terms = do
  MkExecutionContext {node} <- ask
  let RunningNode {networkId, nodeSocket} = node
  scriptAddress' <- scriptAddress script terms
  liftIO $ queryUTxO networkId nodeSocket QueryTip [scriptAddress']

scriptAddress :: AuctionScript -> AuctionTerms -> Runner (Address ShelleyAddr)
scriptAddress script terms = do
  MkExecutionContext {node} <- ask
  return $
    buildScriptAddress
      (PlutusScript $ scriptPlutusScript script terms)
      (networkId node)

queryUTxOByTxInInRunner :: [TxIn] -> Runner UTxO.UTxO
queryUTxOByTxInInRunner txIns = do
  MkExecutionContext {node} <- ask
  liftIO $
    queryUTxOByTxIn (networkId node) (nodeSocket node) QueryTip txIns

fromPlutusAddressInRunner :: PlutusAddress.Address -> Runner AddressInEra
fromPlutusAddressInRunner address' = do
  MkExecutionContext {node} <- ask
  let network = networkIdToNetwork (networkId node)
  return $
    fromPlutusAddress network address'

addressAndKeys ::
  Runner
    ( Address ShelleyAddr
    , VerificationKey PaymentKey
    , SigningKey PaymentKey
    )
addressAndKeys = do
  MkExecutionContext {..} <- ask
  let networkId' = networkId node

  (actorVk, actorSk) <- liftIO $ keysFor actor
  let actorAddress = buildAddress actorVk networkId'

  logMsg $
    "Using actor: " <> show actor <> " with address: " <> show actorAddress

  pure (actorAddress, actorVk, actorSk)

actorTipUtxo :: Runner UTxO.UTxO
actorTipUtxo = do
  MkExecutionContext {node, actor} <- ask
  (vk, _) <- liftIO $ keysFor actor
  liftIO $ queryUTxOFor (networkId node) (nodeSocket node) QueryTip vk
