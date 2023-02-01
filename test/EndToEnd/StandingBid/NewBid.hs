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
import Hydra.Chain.CardanoClient (
  awaitTransaction,
  queryEraHistory,
  queryProtocolParameters,
  queryStakePools,
  querySystemStart,
  submitTransaction,
 )
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

import System.Directory (createDirectoryIfMissing, getCurrentDirectory)

testSuite :: IO TestTree
testSuite = testSpec "StandingBid > NewBid" spec

data Context = MkContext
  { tracer :: Tracer IO EndToEndLog
  , node :: RunningNode
  }

type Scenario a = ReaderT Context IO a

getStateDirectory :: IO FilePath
getStateDirectory = do
  currentDirectory <- getCurrentDirectory
  let stateDirectory = currentDirectory </> "node-state"
  createDirectoryIfMissing True stateDirectory
  pure stateDirectory

runScenario :: Scenario () -> IO ()
runScenario scenario = do
  stateDirectory <- getStateDirectory
  withFile (stateDirectory </> "test.log") ReadWriteMode $ \h ->
    withTracerOutputTo h "Tracer" $ \tracer ->
      withCardanoNodeDevnet
        (contramap FromCardanoNode tracer)
        stateDirectory
        $ \node -> runReaderT scenario (MkContext tracer node)

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
  pure $
    buildScriptAddress
      (PlutusScript $ standingBidValidatorScript terms)
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

  (_, sk) <- liftIO $ keysFor actor

  initTxBody <- mkInitTxBody
  let unbalancedTx =
        initTxBody
          & addVkInputs [txIn]
          & addOutputs [mkAuctionStateOut node]

  liftIO $ writeLog "Unbalanced tx body:" unbalancedTx
  balancingResult <- balanceTx actor utxo unbalancedTx
  liftIO $ case balancingResult of
    Left err -> fail $ show err
    Right balancedTx -> do
      let bTxBody = balancedTxBody balancedTx
      writeLog "Balanced tx body:" bTxBody

      let wit = makeShelleyKeyWitness bTxBody (WitnessPaymentKey sk)
          tx = makeSignedTransaction [wit] bTxBody

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
            (standingBidValidatorScript terms)
        )
        (lovelaceToValue 2_000_000)
        (TxOutDatumInline $ fromPlutusData stateDatum)
        ReferenceScriptNone

-- FIXME: Merge with Scenario context
data NodeInfo = MkNodeInfo
  { protocolParams :: ProtocolParameters
  , systemStart :: SystemStart
  , eraHistory :: EraHistory CardanoMode
  , stakePools :: Set PoolId
  }

queryNodeInfo :: Scenario NodeInfo
queryNodeInfo = do
  MkContext {..} <- ask
  liftIO $ do
    let networkId' = networkId node
        nodeSocket' = nodeSocket node

    MkNodeInfo
      <$> queryProtocolParameters networkId' nodeSocket' QueryTip
      <*> querySystemStart networkId' nodeSocket' QueryTip
      <*> queryEraHistory networkId' nodeSocket' QueryTip
      <*> queryStakePools networkId' nodeSocket' QueryTip

mkInitTxBody :: Scenario (TxBodyContent BuildTx)
mkInitTxBody = do
  MkContext {..} <- ask
  liftIO $ do
    protocolParams <-
      queryProtocolParameters
        (networkId node)
        (nodeSocket node)
        QueryTip
    pure $ emptyTxBody {txProtocolParams = BuildTxWith $ Just protocolParams}

balanceTx ::
  Actor ->
  UTxO ->
  TxBodyContent BuildTx ->
  Scenario
    ( Either
        TxBodyErrorAutoBalance
        BalancedTxBody
    )
balanceTx actor utxoSet unbalancedTxBodyContent = do
  MkNodeInfo {..} <- queryNodeInfo
  actorAddr <- actorAddress actor
  pure $
    makeTransactionBodyAutoBalance
      BabbageEraInCardanoMode
      systemStart
      eraHistory
      protocolParams
      stakePools
      (UTxO.toApi utxoSet)
      unbalancedTxBodyContent
      (ShelleyAddressInEra actorAddr)
      Nothing

standingBidValidatorWitness ::
  AuctionTerms -> BuildTxWith BuildTx (Witness WitCtxTxIn)
standingBidValidatorWitness terms =
  BuildTxWith $ ScriptWitness scriptWitnessCtx script
  where
    script =
      mkScriptWitness
        (standingBidValidatorScript terms)
        InlineScriptDatum
        (toScriptData NewBid)

standingBidValidatorScript :: AuctionTerms -> PlutusScript
standingBidValidatorScript =
  fromPlutusScript . getValidator . standingBidValidator

newBid :: Actor -> AuctionTerms -> Integer -> Scenario ()
newBid actor terms amount = do
  MkContext {..} <- ask
  actorUtxo <- actorTipUtxo actor
  auctionUtxo <- standingBidValidatorTipUtxo terms

  (_, sk) <- liftIO $ keysFor actor
  actorPkh <- liftIO $ actorPubKeyHash actor

  let (actorTxIn, _) = fromJust $ viaNonEmpty head $ UTxO.pairs actorUtxo
      (collateralTxIn, _) = fromJust $ viaNonEmpty last $ UTxO.pairs actorUtxo
      (auctionTxIn, _) = fromJust $ viaNonEmpty head $ UTxO.pairs auctionUtxo

  let addr = standingBidAddress terms
  liftIO $ writeLog "Standing bid address" (unStandingBidAddress addr)

  initTxBody <- mkInitTxBody
  let unbalancedTx =
        initTxBody
          & addVkInputs [actorTxIn]
          & addInputs
            [(auctionTxIn, standingBidValidatorWitness terms)]
          & addOutputs [mkAuctionStateOut node actorPkh]
          & addCollateral collateralTxIn

  liftIO $ writeLog "Auction UTXO" auctionUtxo

  liftIO $ writeLog "Unbalanced tx body:" unbalancedTx
  balancingResult <- balanceTx actor (actorUtxo <> auctionUtxo) unbalancedTx
  liftIO $ case balancingResult of
    Left err -> fail $ show err
    Right balancedTx -> do
      let bTxBody = balancedTxBody balancedTx
      writeLog "Balanced tx body:" bTxBody

      let wit = makeShelleyKeyWitness bTxBody (WitnessPaymentKey sk)
          tx = makeSignedTransaction [wit] bTxBody

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
        (lovelaceToValue 2_000_000)
        (TxOutDatumHash $ hashDatum $ newStateDatum pkh)
        ReferenceScriptNone

addCollateral :: TxIn -> TxBodyContent build -> TxBodyContent build
addCollateral collateralTxIn txBuilder =
  txBuilder
    { txInsCollateral =
        TxInsCollateral $
          collateralTxIn :
          txInsCollateral'
            (txInsCollateral txBuilder)
    }

spec :: Spec
spec = specify "Test E2E" $ do
  runScenario $ do
    initWallet Alice 100_000_000
    terms <- auctionTerms Alice

    initStandingBidState Alice terms
    newBid Alice terms 100
