{-# LANGUAGE RecordWildCards #-}

module EndToEnd.Ledger (testSuite) where

import Prelude

import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX qualified as POSIXTime
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
  AuctionTermsConfig (..),
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
  MkExecutionContext {..} <- ask

  let seller = Alice
      buyer = Bob
      config =
        AuctionTermsConfig
          { deltaBiddingStart = 0
          , deltaBiddingEnd = 2
          , deltaVoucherExpiry = 5
          , deltaCleanup = 10
          , configAuctionFee = 4_000_000
          , configStartingBid = 8_000_000
          , configMinIncrement = 8_000_000
          }

  initWallet seller 100_000_000
  initWallet buyer 100_000_000

  liftIO $ do
    nftTx <- mintOneTestNFT node seller
    let utxoRef = mkTxIn nftTx 0

    currentTime <- round <$> POSIXTime.getPOSIXTime
    terms <- constructTerms node currentTime config seller utxoRef

    announceAuction node seller terms
    startBidding node seller terms
    newBid node buyer terms (fromJust $ intToNatural 16_000_000)
    wait 3
    bidderBuys node buyer terms

sellerReclaimsTest :: Assertion
sellerReclaimsTest = mkAssertion $ do
  MkExecutionContext {..} <- ask

  let seller = Alice
      buyer = Bob
      config =
        AuctionTermsConfig
          { deltaBiddingStart = 0
          , deltaBiddingEnd = 2
          , deltaVoucherExpiry = 5
          , deltaCleanup = 10
          , configAuctionFee = 4_000_000
          , configStartingBid = 8_000_000
          , configMinIncrement = 8_000_000
          }

  initWallet seller 100_000_000
  initWallet buyer 100_000_000

  liftIO $ do
    nftTx <- mintOneTestNFT node seller
    let utxoRef = mkTxIn nftTx 0

    currentTime <- round <$> POSIXTime.getPOSIXTime
    terms <- constructTerms node currentTime config seller utxoRef

    announceAuction node seller terms
    startBidding node seller terms
    wait 6
    sellerReclaims node seller terms

wait :: Int -> IO ()
wait n = threadDelay $ n * 1_000_000
