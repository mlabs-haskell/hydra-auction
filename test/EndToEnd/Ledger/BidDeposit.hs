module EndToEnd.Ledger.BidDeposit (testSuite) where

-- Prelude imports
import Hydra.Prelude (MonadIO (liftIO), SomeException, fail)
import PlutusTx.Prelude

-- Haskell imports
import Control.Monad.Catch (try)
import Data.Maybe (fromJust)

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Tx.Deposit (
  cleanupDeposit,
  losingBidderClaimDeposit,
  mkDeposit,
  sellerClaimDepositFor,
 )
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  startBidding,
 )
import HydraAuction.Tx.StandingBid (cleanupTx, newBid, sellerSignatureForActor)
import HydraAuction.Tx.TermsConfig (
  nonExistentHeadIdStub,
 )
import HydraAuction.Types (AuctionTerms (..))
import HydraAuctionUtils.Fixture (Actor (..), getActorPubKeyHash)
import HydraAuctionUtils.L1.Runner (
  initWallet,
  withActor,
 )
import HydraAuctionUtils.Monads (waitUntil)
import HydraAuctionUtils.Types.Natural (Natural, intToNatural)

-- Hydra auction test imports

import EndToEnd.Ledger.L1Steps (createTermsWithTestNFT)
import EndToEnd.Utils (assertNFTNumEquals, assertUTxOsInScriptEquals, config, mkAssertion)

testSuite :: TestTree
testSuite =
  testGroup
    "Ledger - BidDeposit"
    [ testCase "losing-bidder" losingBidderClaimDepositTest
    , testCase "losing-bidder-double-claim" losingBidderDoubleClaimTest
    , testCase "seller-claims" sellerClaimsDepositTest
    , testCase "seller-claims-losing-deposit" sellerClaimsLosingDepositTest
    , testCase "bidder-buys-with-deposit" bidderBuysWithDepositTest
    , testCase "cleanup-deposit" cleanupDepositTest
    ]

depositAmount :: Natural
depositAmount = fromJust $ intToNatural 20_000_000

losingBidderClaimDepositTest :: Assertion
losingBidderClaimDepositTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

  terms <- createTermsWithTestNFT config nonExistentHeadIdStub

  announceAuction terms

  withActor buyer1 $ mkDeposit terms depositAmount
  withActor buyer2 $ mkDeposit terms depositAmount

  assertUTxOsInScriptEquals Deposit terms 2

  waitUntil $ biddingStart terms
  startBidding terms

  assertNFTNumEquals seller 0

  buyer1SellerSignature <- liftIO $ sellerSignatureForActor terms buyer1
  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2
  withActor buyer1 $ newBid terms (startingBid terms) buyer1SellerSignature
  withActor buyer2 $ newBid terms (startingBid terms + minimumBidIncrement terms) buyer2SellerSignature

  waitUntil $ biddingEnd terms

  withActor buyer1 $ losingBidderClaimDeposit terms

  assertUTxOsInScriptEquals Deposit terms 1

losingBidderDoubleClaimTest :: Assertion
losingBidderDoubleClaimTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

  terms <- createTermsWithTestNFT config nonExistentHeadIdStub

  announceAuction terms

  withActor buyer1 $ mkDeposit terms depositAmount
  withActor buyer2 $ mkDeposit terms depositAmount

  assertUTxOsInScriptEquals Deposit terms 2

  waitUntil $ biddingStart terms
  startBidding terms

  assertNFTNumEquals seller 0

  buyer1SellerSignature <- liftIO $ sellerSignatureForActor terms buyer1
  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2
  withActor buyer1 $ newBid terms (startingBid terms) buyer1SellerSignature
  withActor buyer2 $ newBid terms (startingBid terms + minimumBidIncrement terms) buyer2SellerSignature

  waitUntil $ biddingEnd terms

  withActor buyer1 $ losingBidderClaimDeposit terms

  assertUTxOsInScriptEquals Deposit terms 1

  result <- try $ withActor buyer1 $ losingBidderClaimDeposit terms

  case result of
    Left (_ :: SomeException) -> assertUTxOsInScriptEquals Deposit terms 1
    Right _ -> fail "User should not be able to claim depoist twice"

sellerClaimsDepositTest :: Assertion
sellerClaimsDepositTest = mkAssertion $ do
  let seller = Alice
      buyer = Bob

  mapM_ (initWallet 100_000_000) [seller, buyer]

  terms <- createTermsWithTestNFT config nonExistentHeadIdStub

  announceAuction terms

  withActor buyer $ mkDeposit terms depositAmount

  assertUTxOsInScriptEquals Deposit terms 1

  waitUntil $ biddingStart terms

  startBidding terms

  assertNFTNumEquals seller 0

  buyerSellerSignature <- liftIO $ sellerSignatureForActor terms buyer

  withActor buyer $ newBid terms (startingBid terms) buyerSellerSignature

  waitUntil $ voucherExpiry terms

  buyerPKH <- liftIO $ getActorPubKeyHash buyer

  sellerClaimDepositFor terms buyerPKH

  assertUTxOsInScriptEquals Deposit terms 0

sellerClaimsLosingDepositTest :: Assertion
sellerClaimsLosingDepositTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

  terms <- createTermsWithTestNFT config nonExistentHeadIdStub

  announceAuction terms

  withActor buyer1 $ mkDeposit terms depositAmount
  withActor buyer2 $ mkDeposit terms depositAmount

  assertUTxOsInScriptEquals Deposit terms 2

  waitUntil $ biddingStart terms

  startBidding terms

  assertNFTNumEquals seller 0

  buyer1SellerSignature <- liftIO $ sellerSignatureForActor terms buyer1
  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2
  withActor buyer1 $ newBid terms (startingBid terms) buyer1SellerSignature
  withActor buyer2 $ newBid terms (startingBid terms + minimumBidIncrement terms) buyer2SellerSignature

  waitUntil $ voucherExpiry terms

  buyer1PKH <- liftIO $ getActorPubKeyHash buyer1
  result <- try $ sellerClaimDepositFor terms buyer1PKH

  case result of
    Left (_ :: SomeException) -> assertUTxOsInScriptEquals Deposit terms 2
    Right _ -> fail "Seller should not be able to claim deposit from losing bidder"

bidderBuysWithDepositTest :: Assertion
bidderBuysWithDepositTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

  terms <- createTermsWithTestNFT config nonExistentHeadIdStub

  announceAuction terms

  withActor buyer1 $ mkDeposit terms depositAmount
  withActor buyer2 $ mkDeposit terms depositAmount

  assertUTxOsInScriptEquals Deposit terms 2

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
  assertUTxOsInScriptEquals Deposit terms 1

  waitUntil $ cleanup terms
  cleanupTx terms

cleanupDepositTest :: Assertion
cleanupDepositTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

  terms <- createTermsWithTestNFT config nonExistentHeadIdStub

  announceAuction terms

  withActor buyer1 $ mkDeposit terms depositAmount
  withActor buyer2 $ mkDeposit terms depositAmount

  assertUTxOsInScriptEquals Deposit terms 2

  waitUntil $ biddingStart terms
  startBidding terms

  assertNFTNumEquals seller 0

  buyer1SellerSignature <- liftIO $ sellerSignatureForActor terms buyer1
  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2
  withActor buyer1 $ newBid terms (startingBid terms) buyer1SellerSignature
  withActor buyer2 $ newBid terms (startingBid terms + minimumBidIncrement terms) buyer2SellerSignature

  waitUntil $ cleanup terms
  cleanupTx terms

  withActor buyer1 $ cleanupDeposit terms
  assertUTxOsInScriptEquals Deposit terms 1

  withActor buyer2 $ cleanupDeposit terms
  assertUTxOsInScriptEquals Deposit terms 0
