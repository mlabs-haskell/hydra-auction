{-# LANGUAGE RecordWildCards #-}

module EndToEnd.StandingBid.NewBid (testSuite) where

import Control.Monad.Reader

import Cardano.Api
import Cardano.Api.UTxO as UTxO
import CardanoClient (QueryPoint (QueryTip), buildAddress, queryUTxO)

import Data.Maybe

import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api (
  POSIXTime (POSIXTime),
  PubKeyHash (PubKeyHash),
  toBuiltin,
 )

import Plutus.V2.Ledger.Contexts (TxOutRef (TxOutRef))
import System.FilePath ((</>))

import CardanoNode (
  RunningNode (networkId, nodeSocket),
  withCardanoNodeDevnet,
 )

import Hydra.Cardano.Api (toPlutusTxOutRef)
import Hydra.Cluster.Faucet
import Hydra.Cluster.Fixture
import Hydra.Cluster.Util
import Hydra.Logging
import Hydra.Prelude

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

stateDirectoryRelativePath :: FilePath
stateDirectoryRelativePath = "node-state"

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

actorTipUtxo :: Actor -> Scenario UTxO.UTxO
actorTipUtxo actor = do
  MkContext {..} <- ask
  actorAddr <- actorAddress actor
  liftIO $ queryUTxO (networkId node) (nodeSocket node) QueryTip [actorAddr]

auctionTerms :: Actor -> Scenario AuctionTerms
auctionTerms actor = do
  utxoSet <- actorTipUtxo actor
  let (utxoNonce, _) = fromJust $ viaNonEmpty head $ UTxO.pairs utxoSet
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
      , utxoRef = toPlutusTxOutRef utxoNonce -- TODO -- TxOutRef (toShellyTxId txId) (fromIntegral txIx)
      }

spec :: Spec
spec = specify "Test E2E" $ do
  runScenario $ do
    initWallet Alice 100_000_000
    auctionTerms Alice >> pure ()
