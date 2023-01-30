module OnChain.Spec (onChainTests) where

import PlutusTx.Prelude (traceError, check)
import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (RunningNode (..), withCardanoNodeDevnet)
import Cardano.Api.UTxO as UTxO
import HydraAuction.Types
import HydraAuction.Addresses
import HydraAuction.OnChain.Common
import HydraAuction.OnChain
import Plutus.V1.Ledger.Time (POSIXTime(..))
import Cardano.Api (collateralSupportedInEra)
import Hydra.Cardano.Api -- TODO
import Hydra.Cardano.Api (
  txOutAddress,
  txOutValue,
  CardanoMode,
  ConsensusMode (CardanoMode),
  Era,
  EraHistory (EraHistory),
  EraInMode (BabbageEraInCardanoMode),
  ExecutionUnits (..),
  IsShelleyBasedEra (shelleyBasedEra),
  Lovelace,
  ProtocolParameters (protocolParamMaxTxExUnits, protocolParamMaxTxSize),
  ScriptExecutionError (ScriptErrorMissingScript),
  ScriptWitnessIndex,
  SerialiseAsCBOR (serialiseToCBOR),
  StandardCrypto,
  TransactionValidityError,
  Tx,
  TxOut(..),
  UTxO,
  evaluateTransactionExecutionUnits,
  fromLedgerCoin,
  fromLedgerPParams,
  getTxBody,
  shelleyBasedEra,
  toLedgerExUnits,
  toLedgerPParams,
 )
import Hydra.Ledger.Cardano (
  mkSimpleTx,
  )
import Hydra.Ledger.Cardano.Evaluate (
  evaluateTx,
  evaluateTx',
  maxTxExecutionUnits,
  maxTxSize,
  renderEvaluationReportFailures,
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
import System.FilePath ((</>))
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
import CardanoClient -- (queryTipSlotNo, buildAddress, queryUTxO, QueryPoint(QueryTip))
import Hydra.Chain.CardanoClient -- (queryUTxOFor, awaitTransaction, submitTransaction)
import Hydra.Logging (Tracer, withTracerOutputTo)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Prelude qualified
import Data.Maybe (fromJust)
-- import Cardano.Api (createAndValidateTransactionBody)
import Hydra.Ledger.Cardano.Builder (TxBuilder, InvalidTransactionException(..))
-- import Plutus.V1.Ledger.Tx (TxOut(..))

import Hydra.Chain.Direct.Tx (commitTx, mkHeadId, mkInitialOutput)
import Hydra.Cluster.Fixture (
  Actor (Alice, Bob, Carol, Faucet),
  alice,
  aliceSk,
  bob,
  carol,
  cperiod,
  )
import Hydra.Cluster.Faucet (
  FaucetLog,
  Marked (Fuel, Normal),
  publishHydraScriptsAs,
  seedFromFaucet,
  seedFromFaucet_,
 )
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import HydraAuction.OnChain
import HydraAuction.OnChain.StateToken
import Plutus.V1.Ledger.Api (TokenName(..), CurrencySymbol, MintingPolicy, mkMintingPolicyScript)
import Plutus.V1.Ledger.Contexts (ScriptContext)
import HydraAuction.PlutusExtras
import PlutusTx qualified
import Plutus.V1.Ledger.Value (assetClassValue, AssetClass(..))
import PlutusTx.Prelude (emptyByteString)
import qualified Data.ByteString as BS
import qualified Data.Aeson as Aeson
import Plutus.V1.Ledger.Value (AssetClass, assetClass)
import Plutus.V2.Ledger.Api (
  Data,
  MintingPolicy (getMintingPolicy),
  POSIXTime (..),
  PubKeyHash (..),
  TokenName (..),
  fromBuiltin,
  getValidator,
  toBuiltin,
  toBuiltinData,
  toData,
 )
import PlutusTx.IsData.Class (UnsafeFromData (unsafeFromBuiltinData))
import Hydra.Cardano.Api (
  SerialiseAsRawBytes (serialiseToRawBytes),
  fromPlutusScript,
  hashScript,
  pattern PlutusScript,
 )
import Plutus.V1.Ledger.Address (Address, scriptHashAddress)
import Plutus.V1.Ledger.Api (Validator)
import Plutus.V1.Ledger.Scripts (MintingPolicy, unMintingPolicyScript, unValidatorScript)
import Plutus.V1.Ledger.Value (CurrencySymbol (..))
import PlutusTx.Prelude (toBuiltin)

---

onChainTests :: IO TestTree
onChainTests = do
  testSpec "OnChain" spec

workDir = "./temp-cardano-node"

---

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
mkAllowMintingPolicy :: () ->  ScriptContext -> Bool
mkAllowMintingPolicy _ _ = True

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
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) workDir $ \node@RunningNode{nodeSocket, networkId} -> do

            -- let
            --   nodeSocket = "/tmp/nix-shell.u4wSm2/test-cluster3957/pool-1/node.socket"
            --   networkId = Mainnet -- Testnet $ NetworkMagic 764824073

            -- Mint initial ADA

            (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
            -- (aliceCardanoVk, aliceCardanoSk) <- myKeysFor "../plutip/wallets/signing-key-45c20350baa12aa5807479cdb60cf40b89f7cebc709f2fcdc7a70114.skey"
            let initialAmount = 100_000_000

            slot <- queryTipSlotNo networkId nodeSocket
            putStrLn $ show slot

            -- putStrLn $ "Pre-faucet"
            seedFromFaucet_ node aliceCardanoVk initialAmount Normal (contramap FromFaucet tracer)
            putStrLn $ "Post-faucet"

            let aliceAddress = buildAddress aliceCardanoVk networkId
            putStrLn $ show aliceAddress
            -- putStrLn $ "Address :" <> show $ toLedgerAddr aliceAddress

            utxo <- queryUTxOFor networkId nodeSocket QueryTip aliceCardanoVk
            -- let utxo = fromJust $ viaNonEmpty head utxos
            let utxoAddress = fromJust $ viaNonEmpty last $ txOutAddress <$> toList utxo


            -- Mint 1

            putStrLn $ "VK :" <> show aliceCardanoVk
            putStrLn $ "Utxo :" <> show utxo

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
            let (!txIn, _) = fromJust $ viaNonEmpty head $ UTxO.pairs utxo
            let builder =
                      emptyTxBody {
                        txFee = TxFeeExplicit fee,
                        txInsCollateral = (TxInsCollateral [txIn]),
                        txProtocolParams = BuildTxWith $ Just pparams
                        }
                        & addVkInputs [txIn]
                        & addOutputs [txOut1]
                        & mintTokens (fromPlutusScript $ getMintingPolicy allowMintingPolicy) () [(tokenToAsset $ TokenName emptyByteString,  1)]

            -- let unsignedTx = unsafeBuildTransaction builder
            putStrLn "Pre prebody"
            let preBody =
                    TxBodyContent
                      (withWitness <$> toList (UTxO.inputSet utxo))
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
                      (txMintValue builder)
                      TxScriptValidityNone

            body <- either (\x -> (putStrLn $ show x) >> undefined ) (pure) (
                        second balancedTxBody $ makeTransactionBodyAutoBalance
                          BabbageEraInCardanoMode -- TODO
                          systemStart
                          eraHistory
                          pparams
                          stakePools
                          (UTxO.toApi utxo)
                          preBody
                          (ShelleyAddressInEra aliceAddress)
                          Nothing
                        )
            -- let body = getTxBody unsignedTx
            putStrLn $ "Body: " <> show body

            -- let wit = signWith (getTxId (getTxBody unsignedTx)) aliceCardanoSk
            let wit = makeShelleyKeyWitness (body) (WitnessPaymentKey aliceCardanoSk)
            putStrLn "Before"

            -- let scriptWitness = mkScriptWitness (getMintingPolicy allowMintingPolicy) TxOutDatumNone
            let tx = makeSignedTransaction [wit] (body)
            putStrLn "Signed"

            submitTransaction networkId nodeSocket tx
            putStrLn "Submited"

            slot <- queryTipSlotNo networkId nodeSocket
            putStrLn $ show slot
            void $ awaitTransaction networkId nodeSocket tx
            putStrLn "Awaited"

            -- Terms

            !utxo <- queryUTxOFor networkId nodeSocket QueryTip aliceCardanoVk
            putStrLn $ show utxo
            let (!txIn2, _) = fromJust $ viaNonEmpty last $ UTxO.pairs utxo

            let !terms = AuctionTerms {
                auctionLot = adaAssetClass,
                seller = toPlutusKeyHash $ verificationKeyHash aliceCardanoVk,
                delegates = [],
                biddingStart = POSIXTime 0,
                biddingEnd = POSIXTime 100,
                voucherExpiry = POSIXTime 1000,
                cleanup = POSIXTime 0,
                auctionFee = fromJust $ intToNatural 2_000_000,
                startingBid = fromJust $ intToNatural 1,
                minimumBidIncrement = fromJust $ intToNatural 1,
                utxoRef = toPlutusTxOutRef txIn2
              }

            -- Auction init

            let mp = policy terms

            putStrLn "HERE"
            let !atDatum' = AuctionEscrowDatum Announced (VoucherCS $ scriptCurrencySymbol mp)

            putStrLn "HERE"

            let atDatum :: Data
                !atDatum = toData $ toBuiltinData $ atDatum'

            putStrLn "HERE"

            let !x = fromPlutusData atDatum

            putStrLn "HERE 1"

            putStrLn $ show $ scriptCurrencySymbol mp
            putStrLn $ show atDatum'
            putStrLn $ show atDatum

            let atDatumHash :: Hash ScriptData
                !atDatumHash = hashScriptData x

            putStrLn "HERE 2"

            -- -- let x = mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript $ getValidator $ escrowValidator terms

            !pparams <- queryProtocolParameters networkId nodeSocket QueryTip
            !systemStart <- querySystemStart networkId nodeSocket QueryTip
            !eraHistory <- queryEraHistory networkId nodeSocket QueryTip
            !stakePools <- queryStakePools networkId nodeSocket QueryTip

            putStrLn "Pre prebody"
            putStrLn $ show txIn2

            let toMint = txMintValue $ emptyTxBody & mintTokens (fromPlutusScript $ getMintingPolicy (policy terms)) () [(tokenToAsset $ stateTokenKindToTokenName Voucher,  1)]
            let voucherAssetClass = AssetClass (voucherCurrencySymbol terms, (stateTokenKindToTokenName Voucher))

            let !valueOut = (fromPlutusValue $ assetClassValue allowMintingAssetClass 1) <> (fromPlutusValue $ assetClassValue voucherAssetClass 1) <> lovelaceToValue returnedValue1
            putStrLn "Post value out"
            -- let !a = mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript $ getValidator $ escrowValidator terms
            let !txOut3 = TxOut (ShelleyAddressInEra aliceAddress) valueOut (TxOutDatumHash atDatumHash) ReferenceScriptNone

            let !preBody =
                    TxBodyContent
                      (withWitness <$> toList (UTxO.inputSet utxo))
                      (TxInsCollateral [txIn2])
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
                      (toMint)
                      TxScriptValidityNone

            putStrLn "Post prebbody"

            body <- either (\x -> (putStrLn $ show x) >> undefined ) (pure) (
                        second balancedTxBody $ makeTransactionBodyAutoBalance
                          BabbageEraInCardanoMode -- TODO
                          systemStart
                          eraHistory
                          pparams
                          stakePools
                          (UTxO.toApi utxo)
                          preBody
                          (ShelleyAddressInEra aliceAddress)
                          Nothing
                        )
            -- let body = getTxBody unsignedTx
            putStrLn $ "Body: " <> show body

            -- let wit = signWith (getTxId (getTxBody unsignedTx)) aliceCardanoSk
            let wit = makeShelleyKeyWitness (body) (WitnessPaymentKey aliceCardanoSk)
            putStrLn "Before"
            -- let scriptWitness = mkScriptWitness (getMintingPolicy allowMintingPolicy) TxOutDatumNone
            let tx = makeSignedTransaction [wit] (body)
            putStrLn "Signed"

            submitTransaction networkId nodeSocket tx
            putStrLn "Submited 2"
            void $ awaitTransaction networkId nodeSocket tx
            putStrLn "Awaited 2"
            return ()