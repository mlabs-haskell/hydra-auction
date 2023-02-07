{-# LANGUAGE RecordWildCards #-}

module EndToEnd.Scenario (testSuite) where

import Prelude

import Data.Maybe (fromJust)
import Hydra.Cardano.Api (mkTxIn)
import Hydra.Cluster.Fixture (Actor (..))

import Hydra.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

import EndToEnd.Utils
import HydraAuction.Runner
import HydraAuction.Tx.Escrow
import HydraAuction.Tx.StandingBid
import HydraAuction.Tx.TestNFT
import HydraAuction.Types

testSuite :: TestTree
testSuite =
  testGroup
    "L1 ledger tests"
    [ testCase "Successful auction bid" successfulBidTest
    , testCase "Seller reclaims lot" sellerReclaimsTest
    ]

successfulBidTest :: Assertion
successfulBidTest = mkAssertion $ do
  MkExecutionContext {..} <- ask

  let seller = Alice
      buyer = Bob

  initWallet seller 100_000_000
  initWallet buyer 100_000_000

  liftIO $ do
    nftTx <- mintOneTestNFT node seller
    let utxoRef = mkTxIn nftTx 0

    terms <- constructTerms node seller utxoRef

    announceAuction node seller terms
    startBidding node seller terms
    newBid node buyer terms (fromJust $ intToNatural 16_000_000)
    bidderBuys node buyer terms

sellerReclaimsTest :: Assertion
sellerReclaimsTest = mkAssertion $ do
  MkExecutionContext {..} <- ask

  let seller = Alice
      buyer = Bob

  initWallet seller 100_000_000
  initWallet buyer 100_000_000

  liftIO $ do
    nftTx <- mintOneTestNFT node seller
    let utxoRef = mkTxIn nftTx 0

    terms <- constructTerms node seller utxoRef

    announceAuction node seller terms
    startBidding node seller terms
    liftIO $ sellerReclaims node seller terms
