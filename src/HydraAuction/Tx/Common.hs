{-# LANGUAGE RecordWildCards #-}
module HydraAuction.Tx.Common where

import Prelude
import Hydra.Prelude (toList, viaNonEmpty, void)

import qualified Cardano.Api.UTxO as UTxO
import CardanoClient hiding (networkId)
import CardanoNode (
  RunningNode (RunningNode, networkId, nodeSocket),
 )
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Contravariant (contramap)
import Data.Tuple.Extra (first, second)
import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Cluster.Faucet
import Hydra.Cluster.Fixture
import Hydra.Cluster.Util
import HydraAuction.OnChain
import HydraAuction.Types
import HydraNode
import Plutus.V1.Ledger.Value (TokenName (..), flattenValue)
import Plutus.V2.Ledger.Api (
  txOutValue,
  getValidator,
  POSIXTime (POSIXTime),
  PubKeyHash (PubKeyHash),
  fromBuiltin,
  toBuiltin,
 )
import qualified Data.Map as Map

minLovelance :: Lovelace
minLovelance = 2_000_000

tokenToAsset :: TokenName -> AssetName
tokenToAsset (TokenName t) = AssetName $ fromBuiltin t

mintedTokens :: ToScriptData redeemer => PlutusScript -> redeemer -> [(AssetName, Quantity)] -> TxMintValue BuildTx
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

actorTipUtxo :: RunningNode -> Actor -> IO UTxO.UTxO
actorTipUtxo node actor = do
  (vk, _) <- keysFor actor
  liftIO $ queryUTxOFor (networkId node) (nodeSocket node) QueryTip vk

scriptUtxos :: RunningNode -> AuctionScript -> AuctionTerms -> IO UTxO.UTxO
scriptUtxos node@RunningNode {networkId, nodeSocket} script terms = do
  let scriptAddress = buildScriptAddress (PlutusScript $ fromPlutusScript $ getValidator $ scriptValidatorForTerms script terms) networkId
  queryUTxO networkId nodeSocket QueryTip [scriptAddress]

data AutoCreateParams = AutoCreateParams {
    authoredUtxos :: [(SigningKey PaymentKey, UTxO)],
    otherUtxo :: UTxO,
    -- | Nothing means collateral will be chosen automatically from given UTxOs
    witnessedTxIns ::
      [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn))], -- TODO
    collateral :: Maybe TxIn,
    outs :: [TxOut CtxTx],
    toMint :: TxMintValue BuildTx, -- Maybe
    changeAddress :: Address ShelleyAddr
  }

-- autoCreateTx :: RunningNode -> AutoCreateParams -> IO Tx
autoCreateTx (node@RunningNode {networkId, nodeSocket}) (AutoCreateParams{..})  = do
  pparams <- queryProtocolParameters networkId nodeSocket QueryTip
  systemStart <- querySystemStart networkId nodeSocket QueryTip
  eraHistory <- queryEraHistory networkId nodeSocket QueryTip
  stakePools <- queryStakePools networkId nodeSocket QueryTip

  let
    allAuthoredUtxos = foldl (<>) mempty $ map snd authoredUtxos
    txInsToSign = allAuthoredUtxos

  let txInCollateral =
        case collateral of
          Just txIn -> txIn
          Nothing -> let
              pred x = (length <$> flattenValue <$> txOutValue <$> (toPlutusTxOut $ x)) == Just 1
              utxoMoney = UTxO.filter pred allAuthoredUtxos
              (txIn, _) = head $ UTxO.pairs utxoMoney
            in txIn

  let preBody =
        TxBodyContent
          ((withWitness <$> toList (UTxO.inputSet txInsToSign)) <> witnessedTxIns)
          (TxInsCollateral [txInCollateral])
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
        (UTxO.toApi (allAuthoredUtxos <> otherUtxo))
        preBody
        (ShelleyAddressInEra changeAddress)
        Nothing

  let
      makeWitness sk = makeShelleyKeyWitness body (WitnessPaymentKey sk)
      witnesses = map makeWitness (map fst authoredUtxos)

  pure $ makeSignedTransaction witnesses body

autoSubmitAndAwaitTx node@RunningNode {nodeSocket, networkId} params = do
  tx <- autoCreateTx node params
  putStrLn "Signed"

  submitTransaction networkId nodeSocket tx
  putStrLn "Submited"

  void $ awaitTransaction networkId nodeSocket tx
  putStrLn "Awaited"

  putStrLn $ "Created Tx id: " <> (show $ getTxId $ txBody tx)

  return tx