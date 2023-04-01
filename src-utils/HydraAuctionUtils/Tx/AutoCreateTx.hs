{-# LANGUAGE RecordWildCards #-}

module HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoCreateTx,
  autoSubmitAndAwaitTx,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Data.Foldable (Foldable (toList))

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- Plutus imports
import Plutus.V2.Ledger.Api (POSIXTime)

-- Hydra imports
import Hydra.Cardano.Api (
  Address,
  BuildTx,
  BuildTxWith,
  CtxTx,
  KeyWitness,
  PaymentKey,
  ShelleyAddr,
  SigningKey,
  Tx,
  TxBody,
  TxBodyContent,
  TxBodyErrorAutoBalance,
  TxIn,
  TxMintValue,
  TxOut,
  UTxO,
  WitCtxTxIn,
  Witness,
  balancedTxBody,
  getVerificationKey,
  makeShelleyKeyWitness,
  makeSignedTransaction,
  makeTransactionBodyAutoBalance,
  verificationKeyHash,
  withWitness,
  pattern BabbageEraInCardanoMode,
  pattern BuildTxWith,
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

-- HydraAuction imports
import HydraAuctionUtils.Monads (
  BlockchainParams (..),
  MonadBlockchainParams (..),
  MonadSubmitTx,
  MonadTrace,
  logMsg,
  submitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.Utxo (
  filterAdaOnlyUtxo,
 )

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
