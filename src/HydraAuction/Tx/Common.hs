{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Tx.Common where

import Hydra.Prelude (toList, viaNonEmpty, void)
import PlutusTx.Prelude (emptyByteString)
import Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient hiding (networkId)
import CardanoNode (
  RunningNode (RunningNode, networkId, nodeSocket),
 )
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Contravariant (contramap)
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Tuple.Extra (first, second)
import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Cluster.Faucet
import Hydra.Cluster.Fixture
import Hydra.Cluster.Util
import HydraAuction.OnChain
import HydraAuction.Types
import HydraNode
import Plutus.V1.Ledger.Value (CurrencySymbol (..), TokenName (..), flattenValue, symbols)
import Plutus.V2.Ledger.Api (POSIXTime (POSIXTime), PubKeyHash (PubKeyHash), fromBuiltin, getValidator, toBuiltin, toBuiltinData, toData, txOutValue)

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

mkInlineDatum :: ToScriptData datum => datum -> TxOutDatum ctx
mkInlineDatum x = TxOutDatumInline $ fromPlutusData $ toData $ toBuiltinData x

mkInlinedDatumScriptWitness script redeemer = BuildTxWith $ ScriptWitness scriptWitnessCtx $ mkScriptWitness script InlineScriptDatum (toScriptData redeemer)

addressAndKeysFor :: NetworkId -> Actor -> IO (Address ShelleyAddr, VerificationKey PaymentKey, SigningKey PaymentKey)
addressAndKeysFor networkId actor = do
  (actorVk, actorSk) <- keysFor actor
  let actorAddress = buildAddress actorVk networkId
  putStrLn $ "Using actor: " <> show actor <> " with address: " <> show actorAddress
  return (actorAddress, actorVk, actorSk)

filterUtxoByCurrencySymbols :: [CurrencySymbol] -> UTxO -> UTxO
filterUtxoByCurrencySymbols symbolsToMatch = UTxO.filter hasExactlySymbols
  where
    hasExactlySymbols x = (sort <$> symbols <$> txOutValue <$> (toPlutusTxOut $ x)) == (Just $ sort symbolsToMatch)

filterAdaOnlyUtxo :: UTxO -> UTxO
filterAdaOnlyUtxo = filterUtxoByCurrencySymbols [CurrencySymbol emptyByteString]

actorTipUtxo :: RunningNode -> Actor -> IO UTxO.UTxO
actorTipUtxo node actor = do
  (vk, _) <- keysFor actor
  liftIO $ queryUTxOFor (networkId node) (nodeSocket node) QueryTip vk

scriptUtxos :: RunningNode -> AuctionScript -> AuctionTerms -> IO UTxO.UTxO
scriptUtxos node@RunningNode {networkId, nodeSocket} script terms = do
  let scriptAddress = buildScriptAddress (PlutusScript $ fromPlutusScript $ getValidator $ scriptValidatorForTerms script terms) networkId
  queryUTxO networkId nodeSocket QueryTip [scriptAddress]

data AutoCreateParams = AutoCreateParams
  { authoredUtxos :: [(SigningKey PaymentKey, UTxO)]
  , -- | Nothing means collateral will be chosen automatically from given UTxOs
    witnessedUtxos ::
      [(BuildTxWith BuildTx (Witness WitCtxTxIn), UTxO)]
  , collateral :: Maybe TxIn
  , outs :: [TxOut CtxTx]
  , toMint :: TxMintValue BuildTx
  , changeAddress :: Address ShelleyAddr
  }

autoCreateTx :: RunningNode -> AutoCreateParams -> IO Tx
autoCreateTx (node@RunningNode {networkId, nodeSocket}) (AutoCreateParams {..}) = do
  -- FIXME: proper error handling
  pparams <- queryProtocolParameters networkId nodeSocket QueryTip
  body <-
    either (error . show) id
      <$> callBodyAutoBalance
        node
        (allAuthoredUtxos <> allWitnessedUtxos)
        (preBody pparams)
        changeAddress
  pure $ (makeSignedTransaction (signingWitnesses body) body :: Tx)
  where
    allAuthoredUtxos = foldl (<>) mempty $ fmap snd authoredUtxos
    allWitnessedUtxos = foldl (<>) mempty $ fmap snd witnessedUtxos
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
    preBody pparams =
      TxBodyContent
        ((withWitness <$> txInsToSign) <> witnessedTxIns)
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
    makeSignWitness body sk = makeShelleyKeyWitness body (WitnessPaymentKey sk)
    signingWitnesses :: TxBody -> [KeyWitness]
    signingWitnesses body = fmap (makeSignWitness body) (fmap fst authoredUtxos)

callBodyAutoBalance :: RunningNode -> UTxO -> TxBodyContent BuildTx -> Address ShelleyAddr -> IO (Either TxBodyErrorAutoBalance TxBody)
callBodyAutoBalance (node@RunningNode {networkId, nodeSocket}) utxo preBody changeAddress = do
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

autoSubmitAndAwaitTx node@RunningNode {nodeSocket, networkId} params = do
  tx <- autoCreateTx node params
  putStrLn "Signed"

  submitTransaction networkId nodeSocket tx
  putStrLn "Submited"

  void $ awaitTransaction networkId nodeSocket tx
  putStrLn "Awaited"

  putStrLn $ "Created Tx id: " <> (show $ getTxId $ txBody tx)

  return tx
