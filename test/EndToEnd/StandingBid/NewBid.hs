{-# LANGUAGE RecordWildCards #-}

module EndToEnd.StandingBid.NewBid (testSuite) where

import Control.Monad.Reader

import Cardano.Api.UTxO as UTxO
import CardanoClient (
  QueryPoint (QueryTip),
  buildAddress,
  buildScriptAddress,
  queryUTxO,
 )

import Data.Maybe

import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api (
  Data,
  POSIXTime (POSIXTime),
  PubKeyHash (PubKeyHash),
  getValidator,
  toBuiltin,
  toBuiltinData,
  toData,
 )

import System.FilePath ((</>))

import CardanoNode (
  RunningNode (networkId, nodeSocket),
  withCardanoNodeDevnet,
 )

import Hydra.Cardano.Api
import Hydra.Chain.CardanoClient (awaitTransaction, submitTransaction)
import Hydra.Cluster.Faucet
import Hydra.Cluster.Fixture
import Hydra.Cluster.Util
import Hydra.Ledger.Cardano.Builder
import Hydra.Logging
import Hydra.Prelude

import HydraAuction.Addresses
import HydraAuction.OnChain
import HydraAuction.PlutusExtras
import HydraAuction.Types

import HydraNode

import Test.Hydra.Prelude
import Test.Tasty
import Test.Tasty.Hspec

testSuite :: IO TestTree
testSuite = testSpec "StandingBid > NewBid" spec

data Context = MkContext
  { tracer :: Tracer IO EndToEndLog
  , node :: RunningNode
  }

type Scenario a = ReaderT Context IO a

runScenario :: Scenario () -> IO ()
runScenario scenario =
  withFile (workDir </> "test.log") ReadWriteMode $ \h ->
    withTracerOutputTo h "Tracer" $ \tracer ->
      withCardanoNodeDevnet
        (contramap FromCardanoNode tracer)
        workDir
        $ \node -> runReaderT scenario (MkContext tracer node)
  where
    workDir = "."

initWallet :: Actor -> Lovelace -> Scenario ()
initWallet actor amount = do
  MkContext {..} <- ask
  liftIO $ do
    (vk, _) <- keysFor actor
    seedFromFaucet_
      node
      vk
      amount
      Normal
      (contramap FromFaucet tracer)
    writeLog "Finished initiating wallet" vk

fromCardanoPaymentKeyHash :: Hash PaymentKey -> PubKeyHash
fromCardanoPaymentKeyHash paymentKeyHash =
  PubKeyHash $ toBuiltin $ serialiseToRawBytes paymentKeyHash

actorPubKeyHash :: Actor -> IO PubKeyHash
actorPubKeyHash actor = do
  (vk, _) <- keysFor actor
  pure $ fromCardanoPaymentKeyHash $ verificationKeyHash vk

actorAddress :: Actor -> Scenario (Address ShelleyAddr)
actorAddress actor = do
  MkContext {..} <- ask
  liftIO $ do
    (vk, _) <- keysFor actor
    pure $ buildAddress vk (networkId node)

standingBidValidatorAddress :: AuctionTerms -> Scenario (Address ShelleyAddr)
standingBidValidatorAddress terms = do
  MkContext {..} <- ask
  let script = getValidator $ standingBidValidator terms
  pure $
    buildScriptAddress
      (PlutusScript $ fromPlutusScript @PlutusScriptV2 script)
      (networkId node)

actorTipUtxo :: Actor -> Scenario UTxO.UTxO
actorTipUtxo actor = do
  MkContext {..} <- ask
  actorAddr <- actorAddress actor
  liftIO $ queryUTxO (networkId node) (nodeSocket node) QueryTip [actorAddr]

standingBidValidatorTipUtxo :: AuctionTerms -> Scenario UTxO.UTxO
standingBidValidatorTipUtxo terms = do
  MkContext {..} <- ask
  scriptAddr <- standingBidValidatorAddress terms
  liftIO $ queryUTxO (networkId node) (nodeSocket node) QueryTip [scriptAddr]

auctionTerms :: Actor -> Scenario AuctionTerms
auctionTerms actor = do
  utxo <- actorTipUtxo actor
  let (txIn, _) = fromJust $ viaNonEmpty head $ UTxO.pairs utxo
  actorPkh <- liftIO $ actorPubKeyHash actor
  pure $
    AuctionTerms
      { auctionLot = assetClass "aa" "aa"
      , seller = actorPkh
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

voucherCS :: AuctionTerms -> VoucherCS
voucherCS = VoucherCS . scriptCurrencySymbol . policy

hashDatum :: Data -> Hash ScriptData
hashDatum = hashScriptData . fromPlutusData

writeLog :: Show a => String -> a -> IO ()
writeLog msg x = putStrLn msg >> print x

initStandingBidState :: Actor -> AuctionTerms -> Scenario ()
initStandingBidState actor terms = do
  MkContext {..} <- ask
  utxo <- actorTipUtxo actor
  let (txIn, _) = fromJust $ viaNonEmpty head $ UTxO.pairs utxo
  liftIO $ do
    (_, sk) <- keysFor actor
    let unsignedTx = mkUnsignedTx node txIn
        unsignedTxBody = getTxBody unsignedTx
        wit = signWith (getTxId unsignedTxBody) sk
        tx = makeSignedTransaction [wit] unsignedTxBody

    writeLog "Tx body:" unsignedTxBody
    submitTransaction (networkId node) (nodeSocket node) tx
    txUtxo <- awaitTransaction (networkId node) (nodeSocket node) tx
    writeLog "Finished initiating standing bid state" txUtxo
  where
    stateDatum :: Data
    stateDatum =
      toData $
        toBuiltinData $
          StandingBidDatum NoBid (voucherCS terms)

    mkAuctionStateOut node =
      TxOut
        ( mkScriptAddress @PlutusScriptV2
            (networkId node)
            $ fromPlutusScript $
              getValidator $
                standingBidValidator terms
        )
        (lovelaceToValue 98_000_000)
        (TxOutDatumHash $ hashDatum stateDatum)
        ReferenceScriptNone

    mkUnsignedTx node txIn =
      unsafeBuildTransaction $
        emptyTxBody
          & addVkInputs [txIn]
          & addOutputs [mkAuctionStateOut node]
          & addExplicitFee 2_000_000

addExplicitFee :: Lovelace -> TxBodyContent build -> TxBodyContent build
addExplicitFee p txbc = txbc {txFee = TxFeeExplicit p}

addStandingBidScript ::
  AuctionTerms ->
  TxBodyContent build ->
  TxBodyContent build
addStandingBidScript terms txbc =
  txbc
    { txAuxScripts =
        TxAuxScripts
          [ fromJust $
              toScriptInEra BabbageEra $
                toScriptInAnyLang $
                  PlutusScript $ standingBidValidatorScript terms
          ]
    }

standingBidValidatorScript :: AuctionTerms -> PlutusScript
standingBidValidatorScript =
  fromPlutusScript . getValidator . standingBidValidator

newBid :: Actor -> AuctionTerms -> Integer -> Scenario ()
newBid actor terms amount = do
  MkContext {..} <- ask
  actorUtxo <- actorTipUtxo actor
  auctionUtxo <- standingBidValidatorTipUtxo terms
  let (actorTxIn, _) = fromJust $ viaNonEmpty head $ UTxO.pairs actorUtxo
  let (auctionTxIn, _) = fromJust $ viaNonEmpty head $ UTxO.pairs auctionUtxo
  liftIO $ do
    (_, sk) <- keysFor actor
    actorPkh <- actorPubKeyHash actor
    let unsignedTx = mkUnsignedTx node actorPkh actorTxIn auctionTxIn
        unsignedTxBody = getTxBody unsignedTx
        wit = signWith (getTxId unsignedTxBody) sk
        tx = makeSignedTransaction [wit] unsignedTxBody

    writeLog "Tx body:" unsignedTxBody
    submitTransaction (networkId node) (nodeSocket node) tx
    txUtxo <- awaitTransaction (networkId node) (nodeSocket node) tx
    writeLog "Finished making a new bid" txUtxo
  where
    newStateDatum :: PubKeyHash -> Data
    newStateDatum pkh =
      toData $
        toBuiltinData $
          StandingBidDatum
            (Bid $ BidTerms pkh $ fromJust $ intToNatural amount)
            (voucherCS terms)

    mkAuctionStateOut node pkh =
      TxOut
        ( mkScriptAddress @PlutusScriptV2
            (networkId node)
            (standingBidValidatorScript terms)
        )
        (lovelaceToValue 196_000_000)
        (TxOutDatumHash $ hashDatum $ newStateDatum pkh)
        ReferenceScriptNone

    -- FIXME(mbendkowski): add collateral
    mkUnsignedTx node pkh actorTxIn auctionTxIn =
      unsafeBuildTransaction $
        emptyTxBody
          & addVkInputs [actorTxIn]
          & addInputs
            [
              ( auctionTxIn
              , BuildTxWith $ KeyWitness KeyWitnessForSpending
              )
            ]
          & addOutputs [mkAuctionStateOut node pkh]
          & addExplicitFee 2_000_000
          & addStandingBidScript terms

spec :: Spec
spec = specify "Test E2E" $ do
  runScenario $ do
    initWallet Alice 100_000_000
    terms <- auctionTerms Alice

    initStandingBidState Alice terms

    initWallet Alice 100_000_000
    newBid Alice terms 100
