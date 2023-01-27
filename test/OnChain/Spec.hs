{-# OPTIONS_GHC -w #-}

module OnChain.Spec (onChainTests) where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO as UTxO

-- TODO

import CardanoClient (QueryPoint (QueryTip), buildAddress, queryTipSlotNo, queryUTxO)
import CardanoNode (RunningNode (..), withCardanoNodeDevnet)
import Data.Maybe (fromJust)
import Hydra.Cardano.Api

-- import Hydra.Cardano.Api (
--   CardanoMode,
--   ConsensusMode (CardanoMode),
--   Era,
--   EraHistory (EraHistory),
--   EraInMode (BabbageEraInCardanoMode),
--   ExecutionUnits (..),
--   IsShelleyBasedEra (shelleyBasedEra),
--   Lovelace,
--   Hash,
--   PaymentKey,
--   AssetName,
--   ScriptData,
--   PlutusScriptV2,
--   ProtocolParameters (protocolParamMaxTxExUnits, protocolParamMaxTxSize),
--   ScriptExecutionError (ScriptErrorMissingScript),
--   ScriptWitnessIndex,
--   SerialiseAsCBOR (serialiseToCBOR),
--   StandardCrypto,
--   TransactionValidityError,
--   Tx,
--   TxOut (..),
--   UTxO,
--   evaluateTransactionExecutionUnits,
--   fromLedgerCoin,
--   fromLedgerPParams,
--   getTxBody,
--   shelleyBasedEra,
--   toLedgerExUnits,
--   toLedgerPParams,
--   txOutAddress,
--   txOutValue,
--  )
import Hydra.Chain.CardanoClient (awaitTransaction, submitTransaction)
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

-- import Plutus.V1.Ledger.Tx (TxOut(..))

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
import HydraAuction.Addresses
import HydraAuction.OnChain
import HydraAuction.PlutusExtras
import HydraAuction.Types
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

fromCardanoPaymentKeyHash :: Hash PaymentKey -> PubKeyHash
fromCardanoPaymentKeyHash paymentKeyHash = PubKeyHash $ toBuiltin $ serialiseToRawBytes paymentKeyHash

tokenToAsset :: TokenName -> AssetName
tokenToAsset (TokenName t) = AssetName $ fromBuiltin t

onChainTests :: IO TestTree
onChainTests = do
  testSpec "OnChain" spec

workDir = "."

spec :: Spec
spec = do
  specify "Test E2E" $ do
    withFile (workDir </> "test.log") ReadWriteMode $ \hdl ->
      withTracerOutputTo hdl "Test" $ \tracer ->
        withCardanoNodeDevnet (contramap FromCardanoNode tracer) workDir $ \node@RunningNode {nodeSocket, networkId} -> do
          -- Mint initial ADA

          (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
          let initialAmount = 100_000_000
          seedFromFaucet_ node aliceCardanoVk initialAmount Normal (contramap FromFaucet tracer)

          let aliceAddress = buildAddress aliceCardanoVk networkId

          let alicePKH = fromCardanoPaymentKeyHash $ verificationKeyHash (getVerificationKey aliceCardanoSk)
          utxo <- queryUTxO networkId nodeSocket QueryTip [aliceAddress]
          let utxoAddress = fromJust $ viaNonEmpty head $ txOutAddress <$> toList utxo

          let (txIn, txOut) = fromJust $ viaNonEmpty head $ UTxO.pairs utxo

          -- Mint Auction lot
          let auctionLotAC :: AssetClass
              auctionLotAC = assetClass "aa" "aa"

          let testAt :: AuctionTerms
              testAt =
                AuctionTerms
                  { auctionLot = auctionLotAC
                  , seller = alicePKH
                  , delegates = []
                  , biddingStart = POSIXTime 0
                  , biddingEnd = POSIXTime 0
                  , voucherExpiry = POSIXTime 0
                  , cleanup = POSIXTime 0
                  , auctionFee = fromJust $ intToNatural 1
                  , startingBid = fromJust $ intToNatural 100
                  , minimumBidIncrement = fromJust $ intToNatural 10
                  , utxoRef = toPlutusTxOutRef txIn
                  }

          let mp = policy testAt

          let atDatum :: Data
              atDatum = toData $ toBuiltinData $ AuctionEscrowDatum Announced (VoucherCS $ scriptCurrencySymbol mp)

          let atDatumHash :: Hash ScriptData
              atDatumHash = hashScriptData $ fromPlutusData atDatum

          let voucherOut =
                TxOut
                  (mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript $ getValidator $ escrowValidator testAt)
                  (lovelaceToValue 100000)
                  (TxOutDatumHash atDatumHash)
                  ReferenceScriptNone

          let unsignedTx =
                unsafeBuildTransaction $
                  emptyTxBody
                    & addVkInputs [txIn]
                    & addOutputs [voucherOut]
                    & mintTokens (fromPlutusScript $ getMintingPolicy mp) () [(tokenToAsset $ stateTokenKindToTokenName Voucher, 1)]

          -- let fee = Lovelace 167129

          -- let commitOutput = TxOut utxoAddress (lovelaceToValue $ initialAmount - fee) (mkTxOutDatum (1 :: Integer)) ReferenceScriptNone
          -- let tx' = unsafeBuildTransaction $
          --           emptyTxBody { txFee = TxFeeExplicit fee }
          --             & addVkInputs [txIn]
          --             & addOutputs [commitOutput]
          putStrLn $ show $ getTxWitnesses unsignedTx
          let wit = signWith (getTxId (getTxBody unsignedTx)) aliceCardanoSk
          -- let wit = makeShelleyBootstrapWitness networkId (getTxBody tx') aliceCardanoSk
          let tx = makeSignedTransaction [wit] (getTxBody unsignedTx)
          submitTransaction networkId nodeSocket tx
          void $ awaitTransaction networkId nodeSocket tx
