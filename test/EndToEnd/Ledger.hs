module EndToEnd.Ledger (testSuite) where

-- Prelude imports
import Hydra.Prelude (MonadIO (liftIO))
import PlutusTx.Prelude

-- Haskell imports
import Data.Maybe (fromJust)

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- Hydra imports
import Hydra.Cardano.Api (mkTxIn)
import Hydra.Cluster.Fixture (Actor (..))

-- Hydra auction imports
import HydraAuction.Runner (
  initWallet,
 )
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  sellerReclaims,
  startBidding,
 )
import HydraAuction.Tx.StandingBid (cleanupTx, newBid)
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

-- Hydra auction test imports
import EndToEnd.Utils (mkAssertion, waitUntil)

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
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

  let config =
        AuctionTermsConfig
          { configDiffBiddingStart = 2
          , configDiffBiddingEnd = 5
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

  waitUntil $ biddingStart terms
  startBidding seller terms
  newBid buyer1 terms $ startingBid terms
  newBid buyer2 terms $ startingBid terms + minimumBidIncrement terms

  waitUntil $ biddingEnd terms
  bidderBuys buyer2 terms

  waitUntil $ cleanup terms
  cleanupTx seller terms

sellerReclaimsTest :: Assertion
sellerReclaimsTest = mkAssertion $ do
  let seller = Alice

  initWallet 100_000_000 seller

  let config =
        AuctionTermsConfig
          { configDiffBiddingStart = 2
          , configDiffBiddingEnd = 4
          , configDiffVoucherExpiry = 6
          , configDiffCleanup = 8
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

  waitUntil $ biddingStart terms
  startBidding seller terms

  waitUntil $ voucherExpiry terms
  sellerReclaims seller terms

  waitUntil $ cleanup terms
  cleanupTx seller terms
