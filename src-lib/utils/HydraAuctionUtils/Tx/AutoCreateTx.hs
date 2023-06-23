{-# LANGUAGE RecordWildCards #-}

module HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoCreateTx,
  makeSignedTransactionWithKeys,
  callBodyAutoBalance,
  autoSubmitAndAwaitTx,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports

import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (Foldable (toList))
import Data.Maybe (catMaybes)

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Binary (serialize)

-- Plutus imports
import PlutusLedgerApi.V1 (Interval, POSIXTime)

-- Hydra imports
-- Getting orphan for `ToCBOR Tx`

import Hydra.Cardano.Api (
  Address,
  BuildTx,
  BuildTxWith,
  CtxTx,
  ExecutionUnits (..),
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
  bundleProtocolParams,
  evaluateTransactionExecutionUnits,
  getVerificationKey,
  makeShelleyKeyWitness,
  makeSignedTransaction,
  makeTransactionBodyAutoBalance,
  toLedgerEpochInfo,
  txFee,
  verificationKeyHash,
  withWitness,
  pattern BabbageEra,
  pattern BuildTxWith,
  pattern ShelleyAddressInEra,
  pattern TxAuxScriptsNone,
  pattern TxBody,
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
  pattern TxWithdrawalsNone,
  pattern WitnessPaymentKey,
 )
import Hydra.Ledger.Cardano ()
import Hydra.Ledger.Cardano.Evaluate (
  maxCpu,
  maxMem,
  maxTxSize,
  usedExecutionUnits,
 )

-- HydraAuction imports

import HydraAuctionUtils.Fixture (actorFromSk)
import HydraAuctionUtils.Monads (
  BlockchainParams (..),
  MonadBlockchainParams (..),
  MonadSubmitTx,
  MonadTrace,
  TxStat (..),
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
  , validityBound :: Interval POSIXTime
  }

percentOf :: (Real a) => a -> a -> Double
part `percentOf` total =
  100 * realToFrac part / realToFrac total

autoCreateTx ::
  forall m.
  (MonadIO m, MonadFail m, MonadBlockchainParams m, MonadTrace m) =>
  AutoCreateParams ->
  m Tx
-- FIXME: more docs on usage
autoCreateTx (AutoCreateParams {..}) = do
  (lowerBound, upperBound) <- convertValidityBound validityBound

  MkBlockchainParams {protocolParameters} <- queryBlockchainParams
  body <-
    either (\x -> fail $ "Autobalance error: " <> show x) return
      =<< callBodyAutoBalance
        (allSignedUtxos <> allWitnessedUtxos <> referenceUtxo)
        (preBody protocolParameters lowerBound upperBound)
        changeAddress
  let tx = makeSignedTransactionWithKeys allSigningKeys body
  recordStats tx body
  return tx
  where
    allSignedUtxos = foldMap snd signedUtxos
    allWitnessedUtxos = foldMap snd witnessedUtxos
    allSigningKeys = additionalSigners <> (fst <$> signedUtxos)
    txInsToSign = toList (UTxO.inputSet allSignedUtxos)
    witnessedTxIns =
      [ (txIn, witness)
      | (witness, utxo) <- witnessedUtxos
      , txIn <- fst <$> UTxO.pairs utxo
      ]
    -- FIXME: Just collateral never actually work
    txInCollateral =
      case collateral of
        Just txIn -> txIn
        Nothing -> fst $ case UTxO.pairs $ filterAdaOnlyUtxo allSignedUtxos of
          x : _ -> x
          [] -> error "Cannot select collateral, cuz no money utxo was provided"
    -- FIXME: write on all hacks and invariants:
    --        Signing witness, Utxo, zeroed feez
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
            fmap (verificationKeyHash . getVerificationKey) allSigningKeys
        )
        (BuildTxWith $ Just protocolParameters)
        TxWithdrawalsNone
        TxCertificatesNone
        TxUpdateProposalNone
        toMint
        TxScriptValidityNone
    recordStats :: Tx -> TxBody -> m ()
    recordStats tx body = do
      let TxBody content = body
      signingActors <- liftIO $ mapM actorFromSk allSigningKeys
      case txFee content of
        TxFeeExplicit amount ->
          recordTxStat $
            MkTxStat
              { fee = amount
              , signers = catMaybes signingActors
              }
      -- FIXME: parametrize and use proper logs filtering
      when False traceStats
      where
        evaluateTx = do
          MkBlockchainParams {protocolParameters, systemStart, eraHistory} <-
            queryBlockchainParams
          return $
            evaluateTransactionExecutionUnits
              systemStart
              (toLedgerEpochInfo eraHistory)
              (bundleProtocolParams BabbageEra protocolParameters)
              (UTxO.toApi (allSignedUtxos <> allWitnessedUtxos <> referenceUtxo))
              body
        traceStats = do
          let txSize = fromIntegral $ LBS.length $ serialize tx
          logMsg $ "Tx size % of max: " <> show (txSize `percentOf` maxTxSize)
          eUnits <- evaluateTx
          case eUnits of
            Right units' -> do
              let units = usedExecutionUnits units'
              -- TODO: show per script and parse scripts
              logMsg $
                "CPU % of max: "
                  <> show (executionSteps units `percentOf` maxCpu)
              logMsg $
                "Memory % of max: "
                  <> show (executionMemory units `percentOf` maxMem)
            Left evalError ->
              logMsg $ "Tx evaluation failed: " <> show evalError

makeSignedTransactionWithKeys ::
  [SigningKey PaymentKey] ->
  TxBody ->
  Tx
makeSignedTransactionWithKeys keys txBody =
  makeSignedTransaction keyWitnesses txBody
  where
    createWitness key = makeShelleyKeyWitness txBody (WitnessPaymentKey key)
    keyWitnesses = fmap createWitness keys

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
          systemStart
          (toLedgerEpochInfo eraHistory)
          protocolParameters
          stakePools
          (UTxO.toApi utxo)
          preBody
          (ShelleyAddressInEra changeAddress)
          Nothing

autoSubmitAndAwaitTx ::
  (MonadIO m, MonadTrace m, MonadFail m, MonadBlockchainParams m, MonadSubmitTx m) =>
  AutoCreateParams ->
  m Tx
autoSubmitAndAwaitTx params = do
  tx <- autoCreateTx params
  -- FIXME
  _ <- submitAndAwaitTx tx
  return tx
