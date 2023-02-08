module EndToEnd.Ledger (testSuite) where

import Prelude

import Data.Maybe (fromJust)
import Hydra.Cardano.Api (mkTxIn)
import Hydra.Cluster.Fixture (Actor (..))

import Hydra.Prelude (MonadIO (liftIO), MonadReader (ask))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

import EndToEnd.Utils (mkAssertion)
import HydraAuction.Runner (
  ExecutionContext (MkExecutionContext, node, tracer),
  initWallet,
 )
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  constructTerms,
  sellerReclaims,
  startBidding,
 )
import HydraAuction.Tx.StandingBid (newBid)
import HydraAuction.Tx.TestNFT (mintOneTestNFT)
import HydraAuction.Types (intToNatural)

testSuite :: TestTree
testSuite =
  testGroup
    "L1 ledger tests"
    [ testCase "Successful auction bid" successfulBidTest
    , testCase "Seller reclaims lot" sellerReclaimsTest
    ]

successfulBidTest :: Assertion
successfulBidTest = mkAssertion $ do
  let seller = Alice
      buyer = Bob

  initWallet seller 100_000_000
  initWallet buyer 100_000_000

  nftTx <- mintOneTestNFT seller
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ constructTerms seller utxoRef

  announceAuction seller terms
  startBidding seller terms
  newBid buyer terms (fromJust $ intToNatural 16_000_000)
  bidderBuys buyer terms

sellerReclaimsTest :: Assertion
sellerReclaimsTest = mkAssertion $ do
  let seller = Alice
      buyer = Bob

  initWallet seller 100_000_000
  initWallet buyer 100_000_000

  nftTx <- mintOneTestNFT seller
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ constructTerms seller utxoRef

  announceAuction seller terms
  startBidding seller terms
  sellerReclaims seller terms
