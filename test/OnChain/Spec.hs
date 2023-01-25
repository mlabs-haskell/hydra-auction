module OnChain.Spec (onChainTests) where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (RunningNode (..), withCardanoNodeDevnet)
import Cardano.Api.UTxO as UTxO
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
import CardanoClient (queryTipSlotNo, buildAddress, queryUTxO, QueryPoint(QueryTip))
import Hydra.Chain.CardanoClient (awaitTransaction, submitTransaction)
import Hydra.Logging (Tracer, withTracerOutputTo)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Prelude qualified
import Data.Maybe (fromJust)
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


onChainTests :: IO TestTree
onChainTests = do
  testSpec "OnChain" spec

workDir = "."

spec :: Spec
spec = do
  specify "Test E2E" $ do
    withFile (workDir </> "test.log") ReadWriteMode $ \hdl ->
      withTracerOutputTo hdl "Test" $ \tracer ->
        withCardanoNodeDevnet (contramap FromCardanoNode tracer) workDir $ \node@RunningNode{nodeSocket, networkId} -> do
          -- Mint initial ADA

          (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
          let initialAmount = 100_000_000
          seedFromFaucet_ node aliceCardanoVk initialAmount Normal (contramap FromFaucet tracer)

          let aliceAddress = buildAddress aliceCardanoVk networkId

          utxo <- queryUTxO networkId nodeSocket QueryTip [aliceAddress]
          -- let utxo = fromJust $ viaNonEmpty head utxos
          let utxoAddress = fromJust $ viaNonEmpty head $ txOutAddress <$> toList utxo

          -- Doing empty transaction

          let fee = Lovelace 167129
          let (txIn, txOut) = fromJust $ viaNonEmpty head $ UTxO.pairs utxo
          let commitOutput = TxOut utxoAddress (lovelaceToValue $ initialAmount - fee) (mkTxOutDatum (1 :: Integer)) ReferenceScriptNone
          let tx' = unsafeBuildTransaction $
                    emptyTxBody { txFee = TxFeeExplicit fee }
                      & addVkInputs [txIn]
                      & addOutputs [commitOutput]
          putStrLn $ show $ getTxWitnesses tx'
          let wit = signWith (getTxId (getTxBody tx')) aliceCardanoSk
          -- let wit = makeShelleyBootstrapWitness networkId (getTxBody tx') aliceCardanoSk
          let tx = makeSignedTransaction [wit] (getTxBody tx')
          submitTransaction networkId nodeSocket tx
          void $ awaitTransaction networkId nodeSocket tx
