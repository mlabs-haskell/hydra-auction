-- Things that should be merged in Hydra repo later
-- Copy-pasting `commitTx` and modifying it
-- to support script witnessed commited Utxos
module HydraAuction.HydraHacks (createCommitTx) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports
import Data.Function ((&))
import Data.Set qualified as Set

-- Caradno imports
import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (buildScriptAddress)

-- Hydra imports
import Hydra.Cardano.Api (
  AssetId (AssetId),
  AssetName (AssetName),
  BuildTx,
  BuildTxWith (BuildTxWith),
  CtxUTxO,
  Hash,
  IsScriptWitnessInCtx (scriptWitnessInCtx),
  Key (verificationKeyHash),
  NetworkId,
  PaymentKey,
  PlutusScriptV2,
  SerialiseAsRawBytes (serialiseToRawBytes),
  Tx,
  TxBodyContent,
  TxIn,
  TxOut,
  UTxO,
  Value,
  WitCtxTxIn,
  Witness,
  fromPlutusScript,
  mkScriptAddress,
  mkScriptDatum,
  mkScriptReference,
  mkTxOutDatum,
  toPlutusCurrencySymbol,
  toPlutusTxOutRef,
  toScriptData,
  txInsCollateral,
  txOutValue,
  txProtocolParams,
  valueToList,
  pattern PlutusScript,
  pattern ReferenceScriptNone,
  pattern ScriptWitness,
  pattern TxInsCollateral,
  pattern TxOut,
 )
import Hydra.Chain (HeadId (..))
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.Tx (
  headIdToCurrencySymbol,
  mkCommitDatum,
 )
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Initial qualified as Initial
import Hydra.Ledger.Cardano (addReferenceInputs)
import Hydra.Ledger.Cardano.Builder (
  addExtraRequiredSigners,
  addInputs,
  addOutputs,
  addVkInputs,
  emptyTxBody,
 )
import Hydra.Party (Party)

-- HydraAuction imports

import HydraAuctionUtils.Fixture (partyFor)
import HydraAuctionUtils.Hydra.Runner (
  HydraExecutionContext (..),
  HydraRunner,
  runL1RunnerInComposite,
 )
import HydraAuctionUtils.L1.Runner (L1Runner)
import HydraAuctionUtils.Monads (
  BlockchainParams (..),
  MonadBlockchainParams (..),
  MonadNetworkId (..),
  MonadQueryUtxo (queryUtxo),
  UtxoQuery (..),
 )
import HydraAuctionUtils.Monads.Actors (WithActorT, addressAndKeys, askActor)
import HydraAuctionUtils.Tx.AutoCreateTx (callBodyAutoBalance, makeSignedTransactionWithKeys)

-- | Craft a commit transaction which includes the "committed" utxo as a datum.
commitTxBody ::
  NetworkId ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  HeadId ->
  Party ->
  -- | The initial output (sent to each party) which should contain the PT
  -- and is locked by initial script
  (TxIn, TxOut CtxUTxO) ->
  -- | Ada-only utxo to be commited
  UTxO ->
  -- | Script utxo to be commited
  (TxIn, TxOut CtxUTxO, BuildTxWith BuildTx (Witness WitCtxTxIn)) ->
  -- | Input to cover fees from Hydra node key owner
  TxIn ->
  -- Required signer - Hydra node key owner
  Hash PaymentKey ->
  TxBodyContent BuildTx
commitTxBody
  networkId
  scriptRegistry
  headId
  party
  (initialInput, out)
  moneyUtxo
  (scriptInput, scriptOutput, scriptWitness)
  l1FeeInput
  vkh =
    ( emptyTxBody
        & addInputs
          [ (initialInput, initialWitness)
          , (scriptInput, scriptWitness)
          ]
        & addReferenceInputs [initialScriptRef]
        & addVkInputs (l1FeeInput : Set.toList (UTxO.inputSet moneyUtxo))
        & addExtraRequiredSigners [vkh]
        & addOutputs [commitOutput]
    )
      { txInsCollateral = TxInsCollateral [l1FeeInput]
      }
    where
      fullUtxoToCommit =
        UTxO.fromPairs [(scriptInput, scriptOutput)] <> moneyUtxo
      initialWitness =
        BuildTxWith $
          ScriptWitness scriptWitnessInCtx $
            mkScriptReference initialScriptRef initialScript initialDatum initialRedeemer
      initialScript =
        fromPlutusScript @PlutusScriptV2 Initial.validatorScript
      initialScriptRef =
        fst (initialReference scriptRegistry)
      initialDatum =
        mkScriptDatum $ Initial.datum (headIdToCurrencySymbol headId)
      initialRedeemer =
        toScriptData . Initial.redeemer $
          Initial.ViaCommit
            (map (toPlutusTxOutRef . fst) $ UTxO.pairs fullUtxoToCommit)
      commitOutput =
        TxOut commitAddress commitValue commitDatum ReferenceScriptNone
      commitScript =
        fromPlutusScript Commit.validatorScript
      commitAddress =
        mkScriptAddress @PlutusScriptV2 networkId commitScript
      commitValue =
        txOutValue out
          <> foldMap txOutValue fullUtxoToCommit
      commitDatum =
        mkTxOutDatum $
          mkCommitDatum
            party
            fullUtxoToCommit
            (headIdToCurrencySymbol headId)

-- | Find initial Utxo with Participation Token matchin our current actor
findInitialUtxo :: HeadId -> WithActorT L1Runner (TxIn, TxOut CtxUTxO)
findInitialUtxo headId = do
  (_, commitingNodeVk, _) <- addressAndKeys
  let vkh = verificationKeyHash commitingNodeVk

  initialAddress <- formInitialAddress
  initialUtxo <- queryUtxo (ByAddress initialAddress)
  let initialUtxoForCommiter =
        filter
          (valueHasMatchingPT vkh . txOutValue . snd)
          (UTxO.pairs initialUtxo)

  -- Node should be only in one
  [(initialTxIn, initialTxOut)] <- return initialUtxoForCommiter
  return (initialTxIn, initialTxOut)
  where
    formInitialAddress =
      buildScriptAddress
        (PlutusScript $ fromPlutusScript Initial.validatorScript)
        <$> askNetworkId
    valueHasMatchingPT :: Hash PaymentKey -> Value -> Bool
    valueHasMatchingPT vkh val =
      any isAssetWithMatchingPT $ valueToList val
      where
        isAssetWithMatchingPT (x, _) = case x of
          (AssetId policyId (AssetName bs)) ->
            bs == serialiseToRawBytes vkh
              && toPlutusCurrencySymbol policyId == headIdToCurrencySymbol headId
          _ -> False

findInitialScriptRefUtxo :: MonadQueryUtxo m => ScriptRegistry -> m UTxO
findInitialScriptRefUtxo scriptRegistry = do
  queryUtxo (ByTxIns [initialScriptRef])
  where
    initialScriptRef = fst (initialReference scriptRegistry)

createCommitTx ::
  HeadId ->
  (TxIn, TxOut CtxUTxO) ->
  UTxO ->
  ( TxIn
  , TxOut CtxUTxO
  , BuildTxWith BuildTx (Witness WitCtxTxIn)
  ) ->
  HydraRunner Tx
createCommitTx
  headId
  (l1FeeTxIn, l1FeeTxOut)
  adaOnlyUtxoToCommit
  (scriptTxIn, scriptTxOut, scriptTxWitness) = do
    MkHydraExecutionContext {scriptRegistry} <- ask
    runL1RunnerInComposite $ do
      actor <- askActor
      networkId <- askNetworkId

      -- FIXME: DRY
      (commitingNodeAddress, commitingNodeVk, commitingNodeSk) <-
        addressAndKeys
      let commiterVkh = verificationKeyHash commitingNodeVk

      party <- liftIO $ partyFor actor

      initialScriptRefUtxo <- findInitialScriptRefUtxo scriptRegistry
      (initialTxIn, initialTxOut) <- findInitialUtxo headId

      let preTxBody =
            commitTxBody
              networkId
              scriptRegistry
              headId
              party
              (initialTxIn, initialTxOut)
              adaOnlyUtxoToCommit
              (scriptTxIn, scriptTxOut, scriptTxWitness)
              l1FeeTxIn
              commiterVkh

      -- Patching pparams to match one on Hydra ledger
      MkBlockchainParams {protocolParameters} <- queryBlockchainParams
      let patchedPreTxBody =
            preTxBody
              { txProtocolParams = BuildTxWith $ Just protocolParameters
              }

      let utxos =
            UTxO.fromPairs
              [ (initialTxIn, initialTxOut)
              , (scriptTxIn, scriptTxOut)
              , (l1FeeTxIn, l1FeeTxOut)
              ]
              <> adaOnlyUtxoToCommit
              <> initialScriptRefUtxo

      eTxBody <- callBodyAutoBalance utxos patchedPreTxBody commitingNodeAddress
      case eTxBody of
        Left balancingError -> fail $ show balancingError
        Right txBody ->
          return $
            makeSignedTransactionWithKeys [commitingNodeSk] txBody
