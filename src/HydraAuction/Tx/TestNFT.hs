module HydraAuction.Tx.TestNFT where

import Hydra.Prelude
import PlutusTx.Prelude (emptyByteString)

import Cardano.Api.UTxO as UTxO
import CardanoClient
import CardanoNode (RunningNode (..))
import Data.Maybe (fromJust)
import Hydra.Cardano.Api
import Hydra.Chain.CardanoClient
import Hydra.Cluster.Util (keysFor)
import HydraAuction.OnChain.TestNFT
import HydraAuction.Tx.Common hiding (actorAddress)
import Plutus.V1.Ledger.Value (AssetClass (..), assetClassValue)
import Plutus.V2.Ledger.Api (TokenName (..), getMintingPolicy)

autoCreateTx :: RunningNode -> Address ShelleyAddr -> SigningKey PaymentKey -> [TxIn] -> [TxOut CtxTx] -> UTxO -> TxMintValue BuildTx -> IO Tx
autoCreateTx node@RunningNode {nodeSocket, networkId} changeAddress authorSk insCollateral outs utxoToSpend toMint = do
  pparams <- queryProtocolParameters networkId nodeSocket QueryTip
  systemStart <- querySystemStart networkId nodeSocket QueryTip
  eraHistory <- queryEraHistory networkId nodeSocket QueryTip
  stakePools <- queryStakePools networkId nodeSocket QueryTip

  let preBody =
        TxBodyContent
          (withWitness <$> []) -- Map.toList (UTxO.inputSet utxoToSpend))
          (TxInsCollateral insCollateral)
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
  pure $ makeSignedTransaction [witness] body

mintOneTestNFT node@RunningNode {nodeSocket, networkId} actor = do
  (actorVk, actorSk) <- keysFor actor

  let actorAddress = buildAddress actorVk networkId
  putStrLn $ "Using address: " <> show actorAddress

  utxo <- actorTipUtxo node actor

  let !valueOut = fromPlutusValue (assetClassValue allowMintingAssetClass 1) <> lovelaceToValue minLovelance
  let !txOut1 = TxOut (ShelleyAddressInEra actorAddress) valueOut TxOutDatumNone ReferenceScriptNone
  let (!txIn, _) = fromJust $ viaNonEmpty head $ UTxO.pairs utxo
  let toMint = mintedTokens (fromPlutusScript $ getMintingPolicy allowMintingPolicy) () [(tokenToAsset $ TokenName emptyByteString, 1)]

  tx <- autoCreateTx node actorAddress actorSk [txIn] [txOut1] utxo toMint
  putStrLn "Signed"

  submitTransaction networkId nodeSocket tx
  putStrLn "Submited"

  slot <- queryTipSlotNo networkId nodeSocket
  putStrLn $ show slot
  void $ awaitTransaction networkId nodeSocket tx
  putStrLn "Awaited"

-- TODO: print utxo
