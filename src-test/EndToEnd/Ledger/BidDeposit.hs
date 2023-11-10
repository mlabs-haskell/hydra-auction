module EndToEnd.Ledger.BidDeposit (testSuite) where

-- Prelude imports
import HydraAuctionUtils.Prelude hiding ((*), (+))
import PlutusTx.Prelude ((*), (+))

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

-- TODO

import HydraAuctionUtils.Plutus.Interval
import HydraAuctionUtils.Time
import PlutusLedgerApi.V1.Interval
import PlutusLedgerApi.V1.Time (POSIXTime (..))

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
  startBidding,
 )
import HydraAuction.Tx.StandingBid (cleanupTx, newBid, sellerSignatureForActor)
import HydraAuction.Tx.TermsConfig (
  nonExistentHeadIdStub,
 )
import HydraAuction.Types (AuctionTerms (..))
import HydraAuctionUtils.Extras.CardanoApi (Lovelace (..))
import HydraAuctionUtils.Fixture (getActorPubKeyHash)
import HydraAuctionUtils.L1.Runner (
  L1Runner,
  withActor,
 )
import HydraAuctionUtils.Monads (
  slottyNormalize,
  -- toSlotNo,
  waitUntil
 )
import HydraAuctionUtils.Tx.Build (minLovelace)

-- Hydra auction test imports

import EndToEnd.Ledger.L1Steps (
  buyer1,
  buyer2,
  createTermsWithTestNFT,
  inititalAmount,
  performBidderBuysWithDeposit,
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
    "ledger-bid-deposit"
    [ testCase "test-slots" testSlots
    , testCase "losing-bidder" losingBidderClaimDepositTest
    , testCase "losing-bidder-double-claim" losingBidderDoubleClaimTest
    , testCase "seller-claims" sellerClaimsDepositTest
    , testCase "seller-claims-losing-deposit" sellerClaimsLosingDepositTest
    , testCase "bidder-buys-with-deposit" bidderBuysWithDepositTest
    , testCase "cleanup-deposit" cleanupDepositTest
    ]

depositAmount :: Lovelace
depositAmount = Lovelace 20_000_000

makeBiddersDeposits :: HasCallStack => AuctionTerms -> L1Runner ()
makeBiddersDeposits terms = do
  withActor buyer1 $ mkDeposit terms depositAmount
  withActor buyer2 $ mkDeposit terms depositAmount
  withActor buyer1 $
    assertAdaWithoutFeesEquals $
      inititalAmount - depositAmount
  withActor buyer2 $
    assertAdaWithoutFeesEquals $
      inititalAmount - depositAmount
  assertUTxOsInScriptEquals Deposit terms 2

testSlots :: Assertion
testSlots = mkAssertion $ do
  startTime <- liftIO $ currentTimeMilliseconds

  let
    posixForTick i = POSIXTime $ startTime + tick * i
    (from1, to1) = (posixForTick 2, posixForTick 6)
    exampleInterval = rightExclusiveInterval from1 to1

  from2 <- slottyNormalize 0 from1
  from3 <- slottyNormalize 1 from1
  to2 <- slottyNormalize 0 to1

  let translated = rightExclusiveInterval from2 to2
  let translatedFixed = rightExclusiveInterval from3 to2

  liftIO
    $ assertBool "example interval shouldn't contain translated interval"
    $ not $ exampleInterval `contains` translated
  liftIO
    $ assertBool "example interval shouldn't contain fixed translated interval"
    $ exampleInterval `contains` translatedFixed
  where
    slotTime = 100
    tick = slotTime `div` 4

losingBidderClaimDepositTest :: Assertion
losingBidderClaimDepositTest = mkAssertion $ do
  performInit

  terms <-
    withActor seller $
      createTermsWithTestNFT config nonExistentHeadIdStub
  withActor seller $ assertAdaWithoutFeesEquals inititalAmount

  withActor seller $ announceAuction terms
  withActor seller $
    assertAdaWithoutFeesEquals $
      inititalAmount - minLovelace

  makeBiddersDeposits terms

  waitUntil $ biddingStart terms
  withActor seller $ startBidding terms

  assertNFTNumEquals seller 0

  buyer1SellerSignature <- liftIO $ sellerSignatureForActor terms buyer1
  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2
  withActor buyer1 $ newBid terms (startingBid terms) buyer1SellerSignature
  withActor buyer2 $ newBid terms (startingBid terms + minimumBidIncrement terms) buyer2SellerSignature

  waitUntil $ biddingEnd terms

  withActor buyer1 $ losingBidderClaimDeposit terms

  withActor buyer1 $
    assertAdaWithoutFeesEquals inititalAmount
  assertUTxOsInScriptEquals Deposit terms 1

losingBidderDoubleClaimTest :: Assertion
losingBidderDoubleClaimTest = mkAssertion $ do
  performInit
  terms <-
    withActor seller $
      createTermsWithTestNFT config nonExistentHeadIdStub

  withActor seller $ announceAuction terms

  makeBiddersDeposits terms

  waitUntil $ biddingStart terms
  withActor seller $ startBidding terms

  assertNFTNumEquals seller 0

  buyer1SellerSignature <- liftIO $ sellerSignatureForActor terms buyer1
  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2
  withActor buyer1 $ newBid terms (startingBid terms) buyer1SellerSignature
  withActor buyer2 $ newBid terms (startingBid terms + minimumBidIncrement terms) buyer2SellerSignature

  waitUntil $ biddingEnd terms

  withActor buyer1 $ losingBidderClaimDeposit terms
  -- withActor buyer1 $ assertAdaWithoutFeesEquals inititalAmount

  assertUTxOsInScriptEquals Deposit terms 1

  result <- trySome $ withActor buyer1 $ losingBidderClaimDeposit terms

  case result of
    Left _ -> assertUTxOsInScriptEquals Deposit terms 1
    Right _ -> fail "User should not be able to claim depoist twice"

sellerClaimsDepositTest :: Assertion
sellerClaimsDepositTest = mkAssertion $ do
  performInit

  terms <-
    withActor seller $
      createTermsWithTestNFT config nonExistentHeadIdStub

  withActor seller $ announceAuction terms

  withActor buyer1 $ mkDeposit terms depositAmount
  withActor buyer1 $
    assertAdaWithoutFeesEquals $
      inititalAmount - depositAmount

  assertUTxOsInScriptEquals Deposit terms 1

  waitUntil $ biddingStart terms

  withActor seller $ startBidding terms

  assertNFTNumEquals seller 0

  buyerSellerSignature <- liftIO $ sellerSignatureForActor terms buyer1

  withActor buyer1 $ newBid terms (startingBid terms) buyerSellerSignature

  waitUntil $ voucherExpiry terms

  buyerPKH <- liftIO $ getActorPubKeyHash buyer1

  withActor seller $ sellerClaimDepositFor terms buyerPKH
  withActor seller $
    assertAdaWithoutFeesEquals $
      inititalAmount + depositAmount - (minLovelace * 2)

  assertUTxOsInScriptEquals Deposit terms 0

sellerClaimsLosingDepositTest :: Assertion
sellerClaimsLosingDepositTest = mkAssertion $ do
  performInit
  terms <- withActor seller $ createTermsWithTestNFT config nonExistentHeadIdStub

  withActor seller $ announceAuction terms

  makeBiddersDeposits terms

  waitUntil $ biddingStart terms

  withActor seller $ startBidding terms

  assertNFTNumEquals seller 0

  buyer1SellerSignature <- liftIO $ sellerSignatureForActor terms buyer1
  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2
  withActor buyer1 $ newBid terms (startingBid terms) buyer1SellerSignature
  withActor buyer2 $ newBid terms (startingBid terms + minimumBidIncrement terms) buyer2SellerSignature

  waitUntil $ voucherExpiry terms

  buyer1PKH <- liftIO $ getActorPubKeyHash buyer1
  result <- trySome $ withActor seller $ sellerClaimDepositFor terms buyer1PKH

  case result of
    Left _ -> do
      assertUTxOsInScriptEquals Deposit terms 2
      withActor seller $
        assertAdaWithoutFeesEquals $
          inititalAmount - 2 * minLovelace
    Right _ -> fail "Seller should not be able to claim deposit from losing bidder"

bidderBuysWithDepositTest :: Assertion
bidderBuysWithDepositTest = mkAssertion $ do
  performInit
  terms <-
    withActor seller $
      createTermsWithTestNFT config nonExistentHeadIdStub

  withActor seller $ announceAuction terms

  makeBiddersDeposits terms

  waitUntil $ biddingStart terms
  withActor seller $ startBidding terms

  assertNFTNumEquals seller 0

  buyer1SellerSignature <- liftIO $ sellerSignatureForActor terms buyer1
  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2
  withActor buyer1 $ newBid terms (startingBid terms) buyer1SellerSignature
  withActor buyer2 $ newBid terms (startingBid terms + minimumBidIncrement terms) buyer2SellerSignature

  performBidderBuysWithDeposit terms buyer2 depositAmount $
    startingBid terms + minimumBidIncrement terms

  assertUTxOsInScriptEquals Deposit terms 1

  waitUntil $ cleanup terms
  withActor seller $ cleanupTx terms

cleanupDepositTest :: Assertion
cleanupDepositTest = mkAssertion $ do
  performInit
  terms <-
    withActor seller $
      createTermsWithTestNFT config nonExistentHeadIdStub

  withActor seller $ announceAuction terms

  withActor buyer1 $ mkDeposit terms depositAmount
  withActor buyer2 $ mkDeposit terms depositAmount

  assertUTxOsInScriptEquals Deposit terms 2

  waitUntil $ biddingStart terms
  withActor seller $ startBidding terms

  assertNFTNumEquals seller 0

  buyer1SellerSignature <- liftIO $ sellerSignatureForActor terms buyer1
  buyer2SellerSignature <- liftIO $ sellerSignatureForActor terms buyer2
  withActor buyer1 $ newBid terms (startingBid terms) buyer1SellerSignature
  withActor buyer2 $ newBid terms (startingBid terms + minimumBidIncrement terms) buyer2SellerSignature

  waitUntil $ cleanup terms
  withActor seller $ cleanupTx terms

  withActor buyer1 $ cleanupDeposit terms
  assertUTxOsInScriptEquals Deposit terms 1

  withActor buyer2 $ cleanupDeposit terms
  assertUTxOsInScriptEquals Deposit terms 0
