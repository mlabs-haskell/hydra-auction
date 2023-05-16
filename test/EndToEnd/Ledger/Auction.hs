module EndToEnd.Ledger.Auction (testSuite) where

-- Prelude imports
import Hydra.Prelude (MonadIO (liftIO), SomeException, fail)
import PlutusTx.Prelude

-- Haskell imports

import Control.Monad (void)
import Control.Monad.Catch (try)

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- Hydra imports
import Hydra.Cardano.Api (mkTxIn)

-- Hydra auction imports

import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  sellerReclaims,
  startBidding,
 )
import HydraAuction.Tx.StandingBid (cleanupTx, newBid, sellerSignatureForActor)
import HydraAuction.Tx.TermsConfig (
  configToAuctionTerms,
  constructTermsDynamic,
  nonExistentHeadIdStub,
 )
import HydraAuction.Tx.TestNFT (mintOneTestNFT)
import HydraAuction.Types (AuctionTerms (..))
import HydraAuctionUtils.Fixture (Actor (..))
import HydraAuctionUtils.L1.Runner (
  initWallet,
  withActor,
 )
import HydraAuctionUtils.L1.Runner.Time (waitUntil)

-- Hydra auction test imports
import EndToEnd.Utils (assertNFTNumEquals, config, mkAssertion)

testSuite :: TestTree
testSuite =
  testGroup
    "Ledger - Auction"
    [ testCase "bidder-buys" bidderBuysTest
    , testCase "seller-reclaims" sellerReclaimsTest
    , testCase "unauthorised-bidder" unauthorisedBidderTest
    ]

bidderBuysTest :: Assertion
bidderBuysTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

  nftTx <- mintOneTestNFT
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic seller utxoRef nonExistentHeadIdStub
    configToAuctionTerms config dynamicState

  assertNFTNumEquals seller 1

  announceAuction terms

  waitUntil $ biddingStart terms
  startBidding terms

  assertNFTNumEquals seller 0

  buyer1SellerSignature <- liftIO $ sellerSignatureForActor terms buyer1
  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2
  withActor buyer1 $ newBid terms (startingBid terms) buyer1SellerSignature
  withActor buyer2 $ newBid terms (startingBid terms + minimumBidIncrement terms) buyer2SellerSignature

  waitUntil $ biddingEnd terms
  withActor buyer2 $ bidderBuys terms

  assertNFTNumEquals seller 0
  assertNFTNumEquals buyer1 0
  assertNFTNumEquals buyer2 1

  waitUntil $ cleanup terms
  cleanupTx terms

sellerReclaimsTest :: Assertion
sellerReclaimsTest = mkAssertion $ do
  let seller = Alice

  void $ initWallet 100_000_000 seller

  nftTx <- mintOneTestNFT
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic seller utxoRef nonExistentHeadIdStub
    configToAuctionTerms config dynamicState

  assertNFTNumEquals seller 1
  announceAuction terms

  waitUntil $ biddingStart terms
  startBidding terms
  assertNFTNumEquals seller 0

  waitUntil $ voucherExpiry terms
  sellerReclaims terms

  assertNFTNumEquals seller 1

  waitUntil $ cleanup terms
  cleanupTx terms

unauthorisedBidderTest :: Assertion
unauthorisedBidderTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

  nftTx <- mintOneTestNFT
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic seller utxoRef nonExistentHeadIdStub
    configToAuctionTerms config dynamicState

  assertNFTNumEquals seller 1

  announceAuction terms

  waitUntil $ biddingStart terms
  startBidding terms

  assertNFTNumEquals seller 0

  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2

  result <- try $ withActor buyer1 $ newBid terms (startingBid terms) buyer2SellerSignature

  case result of
    Left (_ :: SomeException) -> return ()
    Right _ -> fail "Seller should not be able to claim deposit from losing bidder"
