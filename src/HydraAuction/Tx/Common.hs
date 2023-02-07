{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Tx.Common (
  AutoCreateParams (..),
  filterAdaOnlyUtxo,
  actorTipUtxo,
  addressAndKeysFor,
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
) where

import Hydra.Prelude (toList, void)
import PlutusTx.Prelude (emptyByteString)
import Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.BaseTypes qualified as Cardano
import CardanoClient hiding (networkId)
import CardanoNode (
  RunningNode (RunningNode, networkId, nodeSocket),
 )
import Control.Monad.IO.Class (liftIO)
import Data.Fixed (Fixed (MkFixed))
import Data.List (sort)
import Data.Map qualified as Map
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Tuple.Extra (first)
import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Chain.Direct.TimeHandle (queryTimeHandle, slotFromUTCTime)
import Hydra.Cluster.Fixture
import Hydra.Cluster.Util
import HydraAuction.OnChain
import HydraAuction.Types
import Plutus.V1.Ledger.Value (CurrencySymbol (..), TokenName (..), symbols)
import Plutus.V2.Ledger.Api (POSIXTime (..), ToData, fromBuiltin, getValidator, toBuiltinData, toData, txOutValue)

networkIdToNetwork :: NetworkId -> Cardano.Network
networkIdToNetwork (Testnet _) = Cardano.Testnet
networkIdToNetwork Mainnet = Cardano.Mainnet

minLovelace :: Lovelace
minLovelace = 2_000_000

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

mkInlinedDatumScriptWitness ::
  (ToData a) =>
  PlutusScript ->
  a ->
  BuildTxWith BuildTx (Witness WitCtxTxIn)
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
    hasExactlySymbols x = (sort . symbols . txOutValue <$> toPlutusTxOut x) == Just (sort symbolsToMatch)

filterAdaOnlyUtxo :: UTxO -> UTxO
filterAdaOnlyUtxo = filterUtxoByCurrencySymbols [CurrencySymbol emptyByteString]

actorTipUtxo :: RunningNode -> Actor -> IO UTxO.UTxO
actorTipUtxo node actor = do
  (vk, _) <- keysFor actor
  liftIO $ queryUTxOFor (networkId node) (nodeSocket node) QueryTip vk

scriptUtxos :: RunningNode -> AuctionScript -> AuctionTerms -> IO UTxO.UTxO
scriptUtxos RunningNode {networkId, nodeSocket} script terms = do
  let scriptAddress = buildScriptAddress (PlutusScript $ fromPlutusScript $ getValidator $ scriptValidatorForTerms script terms) networkId
  queryUTxO networkId nodeSocket QueryTip [scriptAddress]

data AutoCreateParams = AutoCreateParams
  { authoredUtxos :: [(SigningKey PaymentKey, UTxO)]
  , -- | Utxo which TxIns will be used as reference inputs
    referenceUtxo :: UTxO
  , -- | Nothing means collateral will be chosen automatically from given UTxOs
    collateral :: Maybe TxIn
  , witnessedUtxos ::
      [(BuildTxWith BuildTx (Witness WitCtxTxIn), UTxO)]
  , outs :: [TxOut CtxTx]
  , toMint :: TxMintValue BuildTx
  , changeAddress :: Address ShelleyAddr
  , validityBound :: (Maybe POSIXTime, Maybe POSIXTime)
  }

toSlotNo :: RunningNode -> POSIXTime -> IO SlotNo
toSlotNo (RunningNode {networkId, nodeSocket}) ptime = do
  -- FIXME: more clear implementation
  timeHandle <- queryTimeHandle networkId nodeSocket
  let ndtime =
        secondsToNominalDiffTime
          -- Converting milliseconds into picoseconds or something like that
          (MkFixed $ getPOSIXTime ptime * 1_000_000_000)
  case slotFromUTCTime timeHandle (posixSecondsToUTCTime ndtime) of
    Left err -> liftIO $ fail (show err)
    Right slotNo -> pure slotNo

autoCreateTx :: RunningNode -> AutoCreateParams -> IO Tx
autoCreateTx node@RunningNode {networkId, nodeSocket} (AutoCreateParams {..}) = do
  pparams <- queryProtocolParameters networkId nodeSocket QueryTip

  let (lowerBound', upperBound') = validityBound
  lowerBound <- case lowerBound' of
    Nothing -> pure TxValidityNoLowerBound
    Just x -> TxValidityLowerBound <$> toSlotNo node x
  upperBound <- case upperBound' of
    Nothing -> pure TxValidityNoUpperBound
    Just x -> TxValidityUpperBound <$> toSlotNo node x

  body <-
    either (\x -> error $ "Autobalance error: " <> show x) id
      <$> callBodyAutoBalance
        node
        (allAuthoredUtxos <> allWitnessedUtxos <> referenceUtxo)
        (preBody pparams lowerBound upperBound)
        changeAddress
  pure $ makeSignedTransaction (signingWitnesses body) body
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
        -- Adding all keys here, cuz other way `txSignedBy` does not see those signatures
        (TxExtraKeyWitnesses $ fmap (verificationKeyHash . getVerificationKey . fst) authoredUtxos)
        (BuildTxWith $ Just pparams)
        TxWithdrawalsNone
        TxCertificatesNone
        TxUpdateProposalNone
        toMint
        TxScriptValidityNone
    makeSignWitness body sk = makeShelleyKeyWitness body (WitnessPaymentKey sk)
    signingWitnesses :: TxBody -> [KeyWitness]
    signingWitnesses body = fmap (makeSignWitness body . fst) authoredUtxos

callBodyAutoBalance :: RunningNode -> UTxO -> TxBodyContent BuildTx -> Address ShelleyAddr -> IO (Either TxBodyErrorAutoBalance TxBody)
callBodyAutoBalance (RunningNode {networkId, nodeSocket}) utxo preBody changeAddress = do
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

autoSubmitAndAwaitTx :: RunningNode -> AutoCreateParams -> IO Tx
autoSubmitAndAwaitTx node@RunningNode {nodeSocket, networkId} params = do
  tx <- autoCreateTx node params
  putStrLn "Signed"

  submitTransaction networkId nodeSocket tx
  putStrLn "Submited"

  void $ awaitTransaction networkId nodeSocket tx
  putStrLn "Awaited"

  putStrLn $ "Created Tx id: " <> show (getTxId $ txBody tx)

  return tx
