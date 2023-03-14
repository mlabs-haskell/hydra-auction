-- Things that should be in Hydra repo later
module HydraAuction.HydraExtras where

-- TODO: qualify
import Hydra.Cardano.Api
import Hydra.Prelude

import Plutus.V1.Ledger.Api (CurrencySymbol)
import Plutus.V2.Ledger.Api (
  CurrencySymbol (CurrencySymbol),
  toBuiltin,
 )

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (
  QueryPoint (QueryTip),
  buildScriptAddress,
  queryProtocolParameters,
  queryUTxO,
  queryUTxOByTxIn,
 )
import CardanoNode (
  RunningNode (
    RunningNode,
    networkId,
    nodeSocket
  ),
 )
import Hydra.Chain.Direct.State (ChainContext (..))

import Hydra.Cardano.Api (TxInsCollateral)
import Hydra.Chain (HeadId (..))
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.Tx (headIdToCurrencySymbol, mkCommitDatum)
import Hydra.Cluster.Fixture (alice)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.Initial qualified as Initial
import Hydra.Ledger.Cardano (addReferenceInputs)
import Hydra.Ledger.Cardano.Builder (
  addExtraRequiredSigners,
  addInputs,
  addOutputs,
  addVkInputs,
  burnTokens,
  emptyTxBody,
  mintTokens,
  setValidityLowerBound,
  setValidityUpperBound,
  unsafeBuildTransaction,
 )
import Hydra.Party (Party, partyFromChain, partyToChain)

import HydraAuction.Fixture (Actor (Alice), keysFor)
import HydraAuction.Tx.Common (callBodyAutoBalance, submitAndAwaitTx)

-- | Craft a commit transaction which includes the "committed" utxo as a datum.
commitTxBody ::
  NetworkId ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  HeadId ->
  Party ->
  (TxIn, TxOut CtxUTxO) ->
  (TxIn, TxOut CtxUTxO, BuildTxWith BuildTx (Witness WitCtxTxIn)) ->
  (TxIn, TxOut CtxUTxO) ->
  (Hash PaymentKey) ->
  [TxIn] ->
  TxBodyContent BuildTx
commitTxBody
  networkId
  scriptRegistry
  headId
  party
  (initialInput, out)
  (scriptInput, scriptOutput, scriptWitness)
  (moneyInput, moneyOutput)
  vkh
  collaterals =
    ( emptyTxBody
        & addInputs
          [(initialInput, initialWitness), (scriptInput, scriptWitness)]
        & addReferenceInputs [initialScriptRef]
        & addVkInputs [moneyInput]
        & addExtraRequiredSigners [vkh]
        & addOutputs [commitOutput]
    )
      { txInsCollateral = TxInsCollateral collaterals
      }
    where
      initialWitness =
        BuildTxWith $
          ScriptWitness scriptWitnessCtx $
            mkScriptReference initialScriptRef initialScript initialDatum initialRedeemer
      initialScript =
        fromPlutusScript @PlutusScriptV2 Initial.validatorScript
      initialScriptRef =
        fst (initialReference scriptRegistry)
      initialDatum =
        mkScriptDatum $ Initial.datum (headIdToCurrencySymbol headId)
      initialRedeemer =
        toScriptData . Initial.redeemer $
          Initial.ViaCommit (Just $ toPlutusTxOutRef scriptInput)
      commitOutput =
        TxOut commitAddress commitValue commitDatum ReferenceScriptNone
      commitScript =
        fromPlutusScript Commit.validatorScript
      commitAddress =
        mkScriptAddress @PlutusScriptV2 networkId commitScript
      commitValue =
        txOutValue out
          <> txOutValue scriptOutput
      commitDatum =
        mkTxOutDatum $ mkCommitDatum party (Just (scriptInput, scriptOutput)) (headIdToCurrencySymbol headId)

submitAndAwaitCommitTx
  node@RunningNode {networkId, nodeSocket}
  headId
  (!ChainContext {ownVerificationKey, scriptRegistry})
  p1
  (commiterUtxo, !commiterSk)
  (!scriptUtxo, !scriptWitness)
  changeAddress = do
    let !headAddress =
          buildScriptAddress
            (PlutusScript $ fromPlutusScript $ Initial.validatorScript)
            networkId

    let initialScriptRef = fst (initialReference scriptRegistry)
    initialScriptRefUtxo <- queryUTxOByTxIn networkId nodeSocket QueryTip [initialScriptRef]

    let !vkh = verificationKeyHash ownVerificationKey
    headUtxo <- queryUTxO networkId nodeSocket QueryTip [headAddress]
    let (!headTxIn, !headTxOut) : _ = (UTxO.pairs headUtxo)

    let [(!scriptTxIn, !scriptTxOut)] = UTxO.pairs scriptUtxo
    let [(commiterMoneyTxIn, commiterMoneyTxOut)] = UTxO.pairs commiterUtxo

    let !prePreTxBody =
          commitTxBody
            networkId
            scriptRegistry
            headId
            p1
            (headTxIn, headTxOut)
            (scriptTxIn, scriptTxOut, scriptWitness)
            (commiterMoneyTxIn, commiterMoneyTxOut)
            vkh
            [commiterMoneyTxIn]

    pparams <-
      queryProtocolParameters networkId nodeSocket QueryTip
    let preTxBody = prePreTxBody {txProtocolParams = (BuildTxWith $ Just pparams)}

    -- FIXME change address
    let utxos = headUtxo <> scriptUtxo <> initialScriptRefUtxo <> commiterUtxo
    !eTxBody <- callBodyAutoBalance node (utxos) (preTxBody) changeAddress

    !txBody <- case eTxBody of
      Left x -> return $ error $ show x
      Right x -> return x

    (_, aliceSk) <- keysFor Alice

    let keyWitnesses =
          [ makeShelleyKeyWitness txBody (WitnessPaymentKey commiterSk)
          , -- FIXME parametirze
            makeShelleyKeyWitness txBody (WitnessPaymentKey aliceSk)
          ]
        !tx = makeSignedTransaction keyWitnesses txBody

    submitAndAwaitTx node tx
