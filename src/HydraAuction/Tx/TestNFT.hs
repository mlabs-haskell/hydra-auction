module HydraAuction.Tx.TestNFT where

import Hydra.Prelude
import PlutusTx.Prelude (emptyByteString)

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient
import CardanoNode (RunningNode (..))
import Data.Maybe (fromJust)
import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Chain.CardanoClient
import Hydra.Cluster.Util (keysFor)
import HydraAuction.OnChain.TestNFT
import HydraAuction.Tx.Common hiding (actorAddress)
import Plutus.V1.Ledger.Value (AssetClass (..), assetClassValue, flattenValue)
import Plutus.V2.Ledger.Api (TokenName (..), getMintingPolicy, txOutValue)

import Hydra.Ledger.Cardano.Builder

-- autoCreateTx :: RunningNode -> Address ShelleyAddr -> SigningKey PaymentKey -> [TxOut CtxTx] -> UTxO -> [_] ->  TxMintValue BuildTx -> IO Tx
autoCreateTx node@RunningNode {nodeSocket, networkId} changeAddress authorSk outs utxoToSpend otherWitnesses toMint = do
  pparams <- queryProtocolParameters networkId nodeSocket QueryTip
  systemStart <- querySystemStart networkId nodeSocket QueryTip
  eraHistory <- queryEraHistory networkId nodeSocket QueryTip
  stakePools <- queryStakePools networkId nodeSocket QueryTip

  -- TODO: filter ADA outs
  let pred x = (length <$> flattenValue <$> txOutValue <$> (toPlutusTxOut $ x)) == Just 1
      utxoMoney = UTxO.filter pred utxoToSpend
      (txIn, _) = fromJust $ viaNonEmpty head $ UTxO.pairs utxoMoney

  -- TODO
  -- let preBody =
  --       emptyTxBody
  --         & addInputs ((withWitness <$> toList (UTxO.inputSet utxoToSpend)) <> otherWitnesses)
  --         & addOutputs outs

  let preBody =
        TxBodyContent
          ((withWitness <$> toList (UTxO.inputSet utxoToSpend)) <> otherWitnesses)
          (TxInsCollateral [txIn])
          TxInsReferenceNone
          outs
          TxTotalCollateralNone
          TxReturnCollateralNone
          (TxFeeExplicit 0)
          (TxValidityNoLowerBound, TxValidityNoUpperBound)
          TxMetadataNone
          TxAuxScriptsNone
          TxExtraKeyWitnessesNone
          (BuildTxWith $ Just pparams)
          TxWithdrawalsNone
          TxCertificatesNone
          TxUpdateProposalNone
          toMint
          TxScriptValidityNone

  -- FIXME: proper error handling
  body <-
    either (error . show) (pure . balancedTxBody) $
      makeTransactionBodyAutoBalance
        BabbageEraInCardanoMode
        systemStart
        eraHistory
        pparams
        stakePools
        (UTxO.toApi utxoToSpend)
        preBody
        (ShelleyAddressInEra changeAddress)
        Nothing

  let witness = makeShelleyKeyWitness body (WitnessPaymentKey authorSk)
  -- TODO
  pure $ makeSignedTransaction [] body

mintOneTestNFT node@RunningNode {nodeSocket, networkId} actor = do
  (actorVk, actorSk) <- keysFor actor

  let actorAddress = buildAddress actorVk networkId
  putStrLn $ "Using actor: " <> show actor <> "with address: " <> show actorAddress

  utxo' <- actorTipUtxo node actor

  let pred x = (length <$> flattenValue <$> txOutValue <$> (toPlutusTxOut $ x)) == Just 1
      utxo = UTxO.filter pred utxo'
      (txIn, _) = fromJust $ viaNonEmpty last $ UTxO.pairs utxo

  let !valueOut = (fromPlutusValue $ assetClassValue allowMintingAssetClass 1) <> lovelaceToValue minLovelance
  let txOut = TxOut (ShelleyAddressInEra actorAddress) valueOut TxOutDatumNone ReferenceScriptNone
  let toMint = (mintedTokens (fromPlutusScript $ getMintingPolicy allowMintingPolicy) () [(tokenToAsset $ TokenName emptyByteString, 1)])

  tx <- autoCreateTx node actorAddress actorSk [txOut] utxo [] toMint
  putStrLn "Signed"

  submitTransaction networkId nodeSocket tx
  putStrLn "Submited"

  slot <- queryTipSlotNo networkId nodeSocket
  putStrLn $ show slot
  void $ awaitTransaction networkId nodeSocket tx
  putStrLn "Awaited"

  putStrLn $ "Created Tx id: " <> (show $ getTxId $ txBody tx)
