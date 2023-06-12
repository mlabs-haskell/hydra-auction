module EndToEnd.Ledger.Auction (testSuite) where

-- Prelude imports
import Hydra.Prelude (MonadIO (liftIO), SomeException, fail)
import PlutusTx.Prelude

-- Haskell imports
import Control.Monad (void)
import Control.Monad.Catch (try)
import Data.Map qualified as Map

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
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
  initWallet,
  withActor,
 )
import HydraAuctionUtils.Monads (waitUntil)

-- Hydra auction test imports
import EndToEnd.Ledger.L1Steps (createTermsWithTestNFT)
import EndToEnd.Utils (assertNFTNumEquals, assertUTxOsInScriptEquals, config, mkAssertion)

testSuite :: TestTree
testSuite =
  testGroup
    "Ledger - Auction"
    [ testCase "bidder-buys" bidderBuysTest
    , testCase "seller-reclaims" sellerReclaimsTest
    , testCase "unauthorised-bidder" unauthorisedBidderTest
    , testCase "distribute-fees" distributeFeeTest
    ]

bidderBuysTest :: Assertion
bidderBuysTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

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
  withActor buyer2 $ newBid terms (startingBid terms + minimumBidIncrement terms) buyer2SellerSignature

  waitUntil $ biddingEnd terms
  withActor buyer2 $ bidderBuys terms

  assertUTxOsInScriptEquals FeeEscrow terms 1

  assertNFTNumEquals seller 0
  assertNFTNumEquals buyer1 0
  assertNFTNumEquals buyer2 1

  waitUntil $ cleanup terms
  withActor seller $ cleanupTx terms

sellerReclaimsTest :: Assertion
sellerReclaimsTest = mkAssertion $ do
  let seller = Alice

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

  waitUntil $ cleanup terms
  withActor seller $ cleanupTx terms

distributeFeeTest :: Assertion
distributeFeeTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) $ [seller, buyer1, buyer2] <> (Map.!) actorsByKind HydraNodeActor

  terms <-
    withActor seller $
      createTermsWithTestNFT config nonExistentHeadIdStub

  withActor seller $ announceAuction terms

  waitUntil $ biddingStart terms
  withActor seller $ startBidding terms

  assertNFTNumEquals seller 0

  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2
  withActor buyer2 $ newBid terms (startingBid terms + minimumBidIncrement terms) buyer2SellerSignature

  waitUntil $ biddingEnd terms
  withActor buyer2 $ bidderBuys terms

  assertUTxOsInScriptEquals FeeEscrow terms 1

  withActor Oscar $ distributeFee terms

  assertNFTNumEquals seller 0
  assertNFTNumEquals buyer1 0
  assertNFTNumEquals buyer2 1

  assertUTxOsInScriptEquals FeeEscrow terms 0

  waitUntil $ cleanup terms
  withActor seller $ cleanupTx terms

unauthorisedBidderTest :: Assertion
unauthorisedBidderTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

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
    try $
      withActor buyer1 $
        newBid terms (startingBid terms) buyer2SellerSignature

  case result of
    Left (_ :: SomeException) -> return ()
    Right _ -> fail "Seller should not be able to claim deposit from losing bidder"
