module EndToEnd.Ledger (testSuite) where

import Prelude

import Data.Maybe (fromJust)
import Hydra.Cardano.Api (mkTxIn)
import Hydra.Cluster.Fixture (Actor (..))

import Hydra.Prelude (MonadIO (liftIO))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

import EndToEnd.Utils (mkAssertion, waitUntil)
import HydraAuction.Runner (
  initWallet,
 )
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  sellerReclaims,
  startBidding,
 )
import HydraAuction.Tx.StandingBid (newBid)
import HydraAuction.Tx.TermsConfig (
  AuctionTermsConfig (
    AuctionTermsConfig,
    configAuctionFeePerDelegate,
    configDiffBiddingEnd,
    configDiffBiddingStart,
    configDiffCleanup,
    configDiffVoucherExpiry,
    configMinimumBidIncrement,
    configStartingBid
  ),
  configToAuctionTerms,
  constructTermsDynamic,
 )
import HydraAuction.Tx.TestNFT (mintOneTestNFT)
import HydraAuction.Types (AuctionTerms (..), intToNatural)

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

  let config =
        AuctionTermsConfig
          { configDiffBiddingStart = 1
          , configDiffBiddingEnd = 4
          , configDiffVoucherExpiry = 8
          , configDiffCleanup = 10
          , configAuctionFeePerDelegate = fromJust $ intToNatural 4_000_000
          , configStartingBid = fromJust $ intToNatural 8_000_000
          , configMinimumBidIncrement = fromJust $ intToNatural 8_000_000
          }

  nftTx <- mintOneTestNFT seller
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic seller utxoRef
    configToAuctionTerms config dynamicState

  announceAuction seller terms

  liftIO $ waitUntil $ biddingStart terms
  startBidding seller terms
  newBid buyer terms (fromJust $ intToNatural 16_000_000)

  liftIO $ waitUntil $ biddingEnd terms
  bidderBuys buyer terms

sellerReclaimsTest :: Assertion
sellerReclaimsTest = mkAssertion $ do
  let seller = Alice
      buyer = Bob

  initWallet seller 100_000_000
  initWallet buyer 100_000_000

  let config =
        AuctionTermsConfig
          { configDiffBiddingStart = 1
          , configDiffBiddingEnd = 3
          , configDiffVoucherExpiry = 4
          , configDiffCleanup = 5
          , configAuctionFeePerDelegate = fromJust $ intToNatural 4_000_000
          , configStartingBid = fromJust $ intToNatural 8_000_000
          , configMinimumBidIncrement = fromJust $ intToNatural 8_000_000
          }

  nftTx <- mintOneTestNFT seller
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic seller utxoRef
    configToAuctionTerms config dynamicState

  announceAuction seller terms

  liftIO $ waitUntil $ biddingStart terms
  startBidding seller terms

  liftIO $ waitUntil $ voucherExpiry terms
  sellerReclaims seller terms
