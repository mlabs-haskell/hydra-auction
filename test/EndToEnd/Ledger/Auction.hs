module EndToEnd.Ledger.Auction (testSuite) where

-- Prelude imports
import HydraAuctionUtils.Prelude (
  HasCallStack,
  MonadIO (liftIO),
  fail,
  trySome,
 )
import PlutusTx.Prelude

-- Haskell imports
import Control.Monad (void)
import Data.Map qualified as Map

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Tx.Escrow (
  announceAuction,
  sellerReclaims,
  startBidding,
 )
import HydraAuction.Tx.FeeEscrow (distributeFee)
import HydraAuction.Tx.StandingBid (cleanupTx, newBid, sellerSignatureForActor)
import HydraAuction.Tx.TermsConfig (
  nonExistentHeadIdStub,
 )
import HydraAuction.Types (AuctionTerms (..))
import HydraAuctionUtils.Fixture (Actor (..), ActorKind (..), actorsByKind)
import HydraAuctionUtils.L1.Runner (
  L1Runner,
  initWallet,
  withActor,
 )
import HydraAuctionUtils.Monads (waitUntil)
import HydraAuctionUtils.Tx.Build (minLovelace)

-- Hydra auction test imports
import EndToEnd.Ledger.L1Steps (
  buyer1,
  buyer2,
  createTermsWithTestNFT,
  inititalAmount,
  natToLovelace,
  performBidderBuys,
  performInit,
  seller,
 )
import EndToEnd.Utils (
  assertAdaWithoutFeesEquals,
  assertNFTNumEquals,
  assertUTxOsInScriptEquals,
  config,
  mkAssertion,
 )

testSuite :: TestTree
testSuite =
  testGroup
    "Ledger - Auction"
    [ testCase "bidder-buys" bidderBuysTest
    , testCase "seller-reclaims" sellerReclaimsTest
    , testCase "unauthorised-bidder" unauthorisedBidderTest
    , testCase "distribute-fees" distributeFeeTest
    ]

performCleanup :: HasCallStack => AuctionTerms -> L1Runner ()
performCleanup terms = do
  waitUntil $ cleanup terms
  withActor seller $ cleanupTx terms

bidderBuysTest :: Assertion
bidderBuysTest = mkAssertion $ do
  performInit

  terms <-
    withActor seller $
      createTermsWithTestNFT config nonExistentHeadIdStub

  withActor seller $ announceAuction terms

  waitUntil $ biddingStart terms
  withActor seller $ startBidding terms

  assertNFTNumEquals seller 0

  buyer1SellerSignature <- liftIO $ sellerSignatureForActor terms buyer1
  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2
  withActor buyer1 $ newBid terms (startingBid terms) buyer1SellerSignature
  let finalBid = startingBid terms + minimumBidIncrement terms
  withActor buyer2 $ newBid terms finalBid buyer2SellerSignature

  performBidderBuys terms buyer2 finalBid

  performCleanup terms
  -- Get back minLovelace for standing bid
  -- FIXME: standing bid bug
  withActor seller $
    assertAdaWithoutFeesEquals $
      inititalAmount - minLovelace + natToLovelace finalBid

sellerReclaimsTest :: Assertion
sellerReclaimsTest = mkAssertion $ do
  void $ initWallet 100_000_000 seller

  terms <-
    withActor seller $
      createTermsWithTestNFT config nonExistentHeadIdStub

  withActor seller $ announceAuction terms

  waitUntil $ biddingStart terms
  withActor seller $ startBidding terms
  assertNFTNumEquals seller 0

  waitUntil $ voucherExpiry terms
  withActor seller $ sellerReclaims terms

  assertNFTNumEquals seller 1
  assertUTxOsInScriptEquals FeeEscrow terms 1

  performCleanup terms

distributeFeeTest :: Assertion
distributeFeeTest = mkAssertion $ do
  performInit

  mapM_ (initWallet 100_000_000) $ (Map.!) actorsByKind HydraNodeActor

  terms <-
    withActor seller $
      createTermsWithTestNFT config nonExistentHeadIdStub

  withActor seller $ announceAuction terms

  waitUntil $ biddingStart terms
  withActor seller $ startBidding terms

  assertNFTNumEquals seller 0

  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2
  withActor buyer2 $ newBid terms (startingBid terms + minimumBidIncrement terms) buyer2SellerSignature

  performBidderBuys terms buyer2 $
    startingBid terms + minimumBidIncrement terms

  withActor Oscar $ distributeFee terms

  -- FIXME: Oscar got fee minus tx fee
  -- withActor Oscar $ checkDelegateGotFee terms
  withActor Rupert $ checkDelegateGotFee terms
  withActor Patricia $ checkDelegateGotFee terms

  performCleanup terms
  where
    checkDelegateGotFee terms =
      assertAdaWithoutFeesEquals $
        inititalAmount + natToLovelace (auctionFeePerDelegate terms)

unauthorisedBidderTest :: Assertion
unauthorisedBidderTest = mkAssertion $ do
  performInit

  terms <-
    withActor seller $
      createTermsWithTestNFT config nonExistentHeadIdStub

  assertNFTNumEquals seller 1

  withActor seller $ announceAuction terms

  waitUntil $ biddingStart terms
  withActor seller $ startBidding terms

  assertNFTNumEquals seller 0

  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2

  result <-
    trySome $
      withActor buyer1 $
        newBid terms (startingBid terms) buyer2SellerSignature

  case result of
    Left _ -> return ()
    Right _ -> fail "Seller should not be able to claim deposit from losing bidder"
