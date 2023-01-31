module OnChain.Spec (onChainTests) where

-- import PlutusTx.Prelude (traceError, check)
import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO as UTxO
import CardanoNode (RunningNode (..), withCardanoNodeDevnet)
import HydraAuction.Addresses
import HydraAuction.OnChain
import HydraAuction.OnChain.Common
import HydraAuction.Types
import Plutus.V1.Ledger.Time (POSIXTime (..))

-- import Cardano.Api (collateralSupportedInEra)
import Hydra.Cardano.Api -- TODO
-- import Hydra.Cardano.Api (
--   txOutAddress,
--   txOutValue,
--   CardanoMode,
--   ConsensusMode (CardanoMode),
--   Era,
--   EraHistory (EraHistory),
--   EraInMode (BabbageEraInCardanoMode),
--   ExecutionUnits (..),
--   IsShelleyBasedEra (shelleyBasedEra),
--   Lovelace,
--   ProtocolParameters (protocolParamMaxTxExUnits, protocolParamMaxTxSize),
--   ScriptExecutionError (ScriptErrorMissingScript),
--   ScriptWitnessIndex,
--   SerialiseAsCBOR (serialiseToCBOR),
--   StandardCrypto,
--   TransactionValidityError,
--   Tx,
--   TxOut(..),
--   UTxO,
--   evaluateTransactionExecutionUnits,
--   fromLedgerCoin,
--   fromLedgerPParams,
--   getTxBody,
--   shelleyBasedEra,
--   toLedgerExUnits,
--   toLedgerPParams,
--  )

import CardanoClient -- (queryTipSlotNo, buildAddress, queryUTxO, QueryPoint(QueryTip))
import Data.Map.Internal qualified as Map
-- (queryUTxOFor, awaitTransaction, submitTransaction)

import Data.Maybe (fromJust)
import Hydra.Chain.CardanoClient
import Hydra.Ledger.Cardano (
  mkSimpleTx,
 )
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
import Hydra.Ledger.Cardano.Evaluate (
  evaluateTx,
  evaluateTx',
  maxTxExecutionUnits,
  maxTxSize,
  renderEvaluationReportFailures,
 )
import Hydra.Logging (Tracer, withTracerOutputTo)
import HydraNode (
  EndToEndLog (FromCardanoNode, FromFaucet),
  HydraClient,
  hydraNodeId,
  input,
  output,
  send,
  waitFor,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
  withNewClient,
 )
import System.FilePath ((</>))
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Prelude qualified

-- import Cardano.Api (createAndValidateTransactionBody)
import Hydra.Ledger.Cardano.Builder (InvalidTransactionException (..), TxBuilder)

-- import Plutus.V1.Ledger.Tx (TxOut(..))

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Hydra.Cardano.Api (
  PlutusScriptV2,
  SerialiseAsRawBytes (serialiseToRawBytes),
  fromPlutusScript,
  hashScript,
  pattern PlutusScript,
 )
import Hydra.Chain.Direct.Tx (commitTx, mkHeadId, mkInitialOutput)
import Hydra.Cluster.Faucet (
  FaucetLog,
  Marked (Fuel, Normal),
  publishHydraScriptsAs,
  seedFromFaucet,
  seedFromFaucet_,
 )
import Hydra.Cluster.Fixture (
  Actor (Alice, Bob, Carol, Faucet),
  alice,
  aliceSk,
  bob,
  carol,
  cperiod,
 )
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import HydraAuction.OnChain
import HydraAuction.OnChain.StateToken
import HydraAuction.PlutusExtras
import Plutus.V1.Ledger.Address (Address, scriptHashAddress)
import Plutus.V2.Ledger.Api (CurrencySymbol, Data, MintingPolicy (getMintingPolicy), POSIXTime (..), PubKeyHash (..), TokenName (..), Validator, fromBuiltin, getValidator, mkMintingPolicyScript, toBuiltin, toBuiltinData, toData)
import Plutus.V2.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Scripts (MintingPolicy, unMintingPolicyScript, unValidatorScript)
import Plutus.V1.Ledger.Value (AssetClass (..), CurrencySymbol (..), assetClass, assetClassValue)
import PlutusTx qualified
import PlutusTx.IsData.Class (UnsafeFromData (unsafeFromBuiltinData))
import PlutusTx.Prelude (emptyByteString, toBuiltin)

---

onChainTests :: IO TestTree
onChainTests = do
  testSpec "OnChain" spec

---

-- | Mint tokens with given plutus minting script and redeemer.TxMintValue
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

tokenToAsset :: TokenName -> AssetName
tokenToAsset (TokenName t) = AssetName $ fromBuiltin t

-- {-# INLINEABLE wrap #-}
-- wrap = \f -> \r p -> check (f (unsafeFromBuiltinData r) (unsafeFromBuiltinData p))

allowMintingPolicy :: MintingPolicy
allowMintingPolicy =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\_ _ -> ()||])

allowMintingCurrencySymbol :: CurrencySymbol
allowMintingCurrencySymbol = scriptCurrencySymbol allowMintingPolicy

{-# INLINEABLE mkAllowMintingPolicy #-}
mkAllowMintingPolicy :: () -> ScriptContext -> Bool
mkAllowMintingPolicy _ _ = True

allowMintingAssetClass :: AssetClass
allowMintingAssetClass = AssetClass (allowMintingCurrencySymbol, (TokenName emptyByteString))

-- -- context of Hydra.
-- unsafeBuildTransaction' :: HasCallStack => TxBuilder -> Tx
-- unsafeBuildTransaction' builder =
--   either
--     (\txBodyError -> bug $ InvalidTransactionException{txBodyError, builder})
--     (`Tx` mempty)
--     . createAndValidateTransactionBody
--     $ builder
---

myKeysFor :: FilePath -> IO (VerificationKey PaymentKey, SigningKey PaymentKey)
myKeysFor path = do
  bs <- BS.readFile path
  let res =
        first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict bs)
          >>= deserialiseFromTextEnvelope asSigningKey
  case res of
    Left err ->
      fail $ "cannot decode text envelope from '" <> show bs <> "', error: " <> show err
    Right sk -> pure (getVerificationKey sk, sk)
  where
    asSigningKey :: AsType (SigningKey PaymentKey)
    asSigningKey = AsSigningKey AsPaymentKey

spec :: Spec
spec = do
  specify "Test E2E" $ do
    withTempDir "hydra-cluster" $ \workDir ->
      withFile (workDir </> "test.log") ReadWriteMode $ \hdl ->
        withTracerOutputTo hdl "Test" $ \tracer -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) workDir $ \node@RunningNode {nodeSocket, networkId} -> do
            res_txMintInit@(aliceCardanoVk, aliceCardanoSk, aliceAddress, aliceUtxoInit, utxoAddress, initialAmount) <- tx_mintInit tracer node

            res_txMint1@(returnedValue1, _) <- tx_mint1 tracer node res_txMintInit

            (auctionTerms, utxoNonce, utxoSet) <- mkAuctionTerms tracer node aliceCardanoVk

            (atDatumHash, pparams, systemStart, eraHistory, stakePools) <- 
               prep_AuctionInit tracer node (aliceCardanoVk, aliceCardanoSk, aliceAddress) returnedValue1 (auctionTerms, utxoNonce, utxoSet)

            tx_AuctionInit tracer node (aliceCardanoVk, aliceCardanoSk, aliceAddress) returnedValue1 (auctionTerms, utxoNonce, utxoSet) (atDatumHash, pparams, systemStart, eraHistory, stakePools)

            pure ()

tx_mintInit tracer node@RunningNode {nodeSocket, networkId} = do
  -- Mint initial ADA

  (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
  -- (aliceCardanoVk, aliceCardanoSk) <- myKeysFor "../plutip/wallets/signing-key-6c6b2aee64fe4b6d88a6e62e8133fe48a44b406789bd8635cde915ba.skey"
  let initialAmount = 100_000_000

  slot <- queryTipSlotNo networkId nodeSocket
  putStrLn $ show slot

  -- putStrLn $ "Pre-faucet"
  seedFromFaucet_ node aliceCardanoVk initialAmount Normal (contramap FromFaucet tracer)
  putStrLn $ "Post-faucet"

  let aliceAddress = buildAddress aliceCardanoVk networkId
  putStrLn $ show aliceAddress
  -- putStrLn $ "Address :" <> show $ toLedgerAddr aliceAddress

  aliceUtxoInit <- queryUTxOFor networkId nodeSocket QueryTip aliceCardanoVk
  -- let utxo = fromJust $ viaNonEmpty head utxos
  let utxoAddress = fromJust $ viaNonEmpty last $ txOutAddress <$> toList aliceUtxoInit

  pure (aliceCardanoVk, aliceCardanoSk, aliceAddress, aliceUtxoInit, utxoAddress, initialAmount)

tx_mint1 tracer node@RunningNode {nodeSocket, networkId} (aliceCardanoVk, aliceCardanoSk, aliceAddress, aliceUtxoInit, utxoAddress, initialAmount) = do
  -- Mint 1

  putStrLn $ "VK :" <> show aliceCardanoVk
  putStrLn $ "Utxo :" <> show aliceUtxoInit

  let fee = Lovelace 172805
  let !returnedValue1 = 2_000_000 :: Lovelace
  let !returnedValue2 = initialAmount - returnedValue1 - fee
  putStrLn $ "Values " <> show returnedValue1 <> " " <> show returnedValue2

  pparams <- queryProtocolParameters networkId nodeSocket QueryTip
  systemStart <- querySystemStart networkId nodeSocket QueryTip
  eraHistory <- queryEraHistory networkId nodeSocket QueryTip
  stakePools <- queryStakePools networkId nodeSocket QueryTip

  let !valueOut = (fromPlutusValue $ assetClassValue allowMintingAssetClass 1) <> lovelaceToValue returnedValue1
  let !txOut1 = TxOut (ShelleyAddressInEra aliceAddress) valueOut TxOutDatumNone ReferenceScriptNone
  let (!txIn, _) = fromJust $ viaNonEmpty head $ UTxO.pairs aliceUtxoInit

  -- let unsignedTx = unsafeBuildTransaction builder
  putStrLn "Pre prebody"
  let preBody =
        TxBodyContent
          (withWitness <$> toList (UTxO.inputSet aliceUtxoInit))
          (TxInsCollateral [txIn])
          TxInsReferenceNone
          [txOut1]
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
          (mintedTokens (fromPlutusScript @PlutusScriptV2 $ getMintingPolicy allowMintingPolicy) () [(tokenToAsset $ TokenName emptyByteString, 1)])
          TxScriptValidityNone

  body <-
    either
      (\x -> (putStrLn $ show x) >> undefined)
      pure
      ( second balancedTxBody $
          makeTransactionBodyAutoBalance
            BabbageEraInCardanoMode -- TODO
            systemStart
            eraHistory
            pparams
            stakePools
            (UTxO.toApi aliceUtxoInit)
            preBody
            (ShelleyAddressInEra aliceAddress)
            Nothing
      )
  -- let body = getTxBody unsignedTx
  putStrLn $ "Body: " <> show body

  -- let wit = signWith (getTxId (getTxBody unsignedTx)) aliceCardanoSk
  let wit = makeShelleyKeyWitness body (WitnessPaymentKey aliceCardanoSk)
  putStrLn "Before"

  -- let scriptWitness = mkScriptWitness (getMintingPolicy allowMintingPolicy) TxOutDatumNone
  let tx = makeSignedTransaction [wit] body
  putStrLn "Signed"

  submitTransaction networkId nodeSocket tx
  putStrLn "Submited"

  slot <- queryTipSlotNo networkId nodeSocket
  putStrLn $ show slot
  void $ awaitTransaction networkId nodeSocket tx
  putStrLn "Awaited"

  pure (returnedValue1, ())

mkAuctionTerms _tracer node@RunningNode {nodeSocket, networkId} aliceCardanoVk = do
  -- Terms

  !utxoSet <- queryUTxOFor networkId nodeSocket QueryTip aliceCardanoVk
  putStrLn $ show utxoSet
  -- TODO CHECK: Is this `viaNonEmpty last` causing headList []?
  let (!utxoNonce, _) = fromJust $ viaNonEmpty last $ UTxO.pairs utxoSet

  let !terms =
        AuctionTerms
          { auctionLot = adaAssetClass
          , seller = toPlutusKeyHash $ verificationKeyHash aliceCardanoVk
          , delegates = []
          , biddingStart = POSIXTime 0
          , biddingEnd = POSIXTime 100
          , voucherExpiry = POSIXTime 1000
          , cleanup = POSIXTime 10001
          , auctionFee = fromJust $ intToNatural 2_000_000
          , startingBid = fromJust $ intToNatural 2_000_000
          , minimumBidIncrement = fromJust $ intToNatural 2_000_000
          , utxoRef = toPlutusTxOutRef utxoNonce
          }

  pure (terms, utxoNonce, utxoSet)

prep_AuctionInit _tracer node@RunningNode {nodeSocket, networkId} (aliceCardanoVk, aliceCardanoSk, aliceAddress) returnedValue1 (auctionTerms, utxoNonce, utxoSet) = do
  -- Auction init

  let mp = policy auctionTerms

  let !atDatum' = AuctionEscrowDatum Announced (VoucherCS $ scriptCurrencySymbol mp)

  let atDatum :: Data
      !atDatum = toData $ toBuiltinData $ atDatum'

  let !atDatum_scriptData = fromPlutusData atDatum

  putStrLn $ show $ scriptCurrencySymbol mp
  putStrLn $ show atDatum'
  putStrLn $ show atDatum

  let atDatumHash :: Hash ScriptData
      !atDatumHash = hashScriptData atDatum_scriptData

  -- -- let x = mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript $ getValidator $ escrowValidator auctionTerms

  !pparams <- queryProtocolParameters networkId nodeSocket QueryTip
  !systemStart <- querySystemStart networkId nodeSocket QueryTip
  !eraHistory <- queryEraHistory networkId nodeSocket QueryTip
  !stakePools <- queryStakePools networkId nodeSocket QueryTip

  pure (atDatumHash, pparams, systemStart, eraHistory, stakePools)



tx_AuctionInit _tracer node@RunningNode {nodeSocket, networkId} (aliceCardanoVk, aliceCardanoSk, aliceAddress) returnedValue1 (auctionTerms, utxoNonce, utxoSet) (atDatumHash, pparams, systemStart, eraHistory, stakePools) = do
  putStrLn "Pre prebody"
  putStrLn $ show utxoNonce

  let voucherAssetClass = AssetClass (voucherCurrencySymbol auctionTerms, (stateTokenKindToTokenName Voucher))

  let !valueOut = (fromPlutusValue $ assetClassValue allowMintingAssetClass 1) <> (fromPlutusValue $ assetClassValue voucherAssetClass 1) <> lovelaceToValue returnedValue1
  putStrLn "Post value out"
  let !a = mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript @PlutusScriptV2 $ getValidator $ escrowValidator auctionTerms
  let !txOut3 = TxOut (a) valueOut (TxOutDatumHash atDatumHash) ReferenceScriptNone

  let !preBody =
        TxBodyContent
          (withWitness <$> toList (UTxO.inputSet utxoSet))
          (TxInsCollateral [utxoNonce])
          TxInsReferenceNone
          [txOut3]
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
          (mintedTokens (fromPlutusScript @PlutusScriptV2 $ getMintingPolicy (policy auctionTerms)) () [(tokenToAsset $ stateTokenKindToTokenName Voucher, 1)])
          TxScriptValidityNone

  putStrLn "Post prebbody"

  -- putStrLn $ "atDatumHash: " <> show atDatumHash
  -- putStrLn $ "pparams: " <> show pparams
  -- putStrLn $ "systemStart: " <> show systemStart
  -- putStrLn $ "eraHistory: " <> show eraHistory
  -- putStrLn $ "stakePools: " <> show stakePools

  -- putStrLn $ "utxo set:" <> show (UTxO.toApi utxoSet)

  -- putStrLn $ "pre body: " <> show preBody

  let !eitherBody = makeTransactionBodyAutoBalance
            BabbageEraInCardanoMode -- TODO
            systemStart
            eraHistory
            pparams
            stakePools
            (UTxO.toApi utxoSet)
            preBody
            (ShelleyAddressInEra aliceAddress)
            Nothing
      !body = either (error . show) balancedTxBody eitherBody

  putStrLn $ "Body: " <> show body

  -- let wit = signWith (getTxId (getTxBody unsignedTx)) aliceCardanoSk
  let wit = makeShelleyKeyWitness body (WitnessPaymentKey aliceCardanoSk)
  putStrLn "Before"
  -- let scriptWitness = mkScriptWitness (getMintingPolicy allowMintingPolicy) TxOutDatumNone
  let tx = makeSignedTransaction [wit] body
  putStrLn "Signed"

  submitTransaction networkId nodeSocket tx
  putStrLn "Submited 2"
  void $ awaitTransaction networkId nodeSocket tx
  putStrLn "Awaited 2"
