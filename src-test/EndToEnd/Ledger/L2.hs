module EndToEnd.Ledger.L2 (testSuite) where

-- Prelude import
import HydraAuctionUtils.Prelude

-- Haskell imports

import Data.Map qualified as Map

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- Hydra auction imports

import HydraAuction.Delegate.Interface (
  AbortReason (..),
  DelegateResponse (..),
  DelegateState (..),
  InitializedState (..),
  ResponseReason (..),
 )
import HydraAuction.Tx.Escrow (
  bidderBuys,
 )
import HydraAuction.Tx.StandingBid (newBid, sellerSignatureForActor)
import HydraAuction.Tx.TermsConfig (AuctionTermsConfig (..))
import HydraAuction.Types (AuctionTerms (..))
import HydraAuctionUtils.Delegate.Logic (abort)
import HydraAuctionUtils.Fixture (Actor (..), ActorKind (..), actorsByKind)
import HydraAuctionUtils.L1.Runner (
  initWallet,
  withActor,
 )
import HydraAuctionUtils.Monads (waitUntil)
import HydraAuctionUtils.WebSockets.Client (AwaitedOutput (..))

-- Hydra auction test imports
import EndToEnd.HydraUtils (
  EmulatorDelegate (..),
  runCompositeForAllDelegates,
  runCompositeForDelegate,
  runEmulatorInTest,
  runL1InEmulator,
 )
import EndToEnd.Ledger.L1Steps (
  announceAndStartBidding,
  correctBidNo,
  createTermsWithTestNFT,
 )
import EndToEnd.Ledger.L2Steps
import EndToEnd.Utils (assertNFTNumEquals, mkAssertionOfIO)
import EndToEnd.Utils qualified as Utils

testSuite :: TestTree
testSuite =
  testGroup
    "ledger-l2"
    [ testCase "bidder-buys" bidderBuysTest
    , testCase "multiple-utxos-to-commit" multipleUtxosToCommitTest
    , testCase "early-abort" earlyAbort
    , testCase "late-abort" lateAbort
    ]

config :: AuctionTermsConfig
config =
  Utils.config
    { -- L2 stuff testing test take time after bidding end
      configDiffBiddingEnd = 24
    , configDiffVoucherExpiry = 26
    , configDiffCleanup = 28
    }

-- Includes testing L1 biding before and after L2 moves
-- Inculdes testing of placing bid by same delegate who moved standing bid
bidderBuysTest :: HasCallStack => Assertion
bidderBuysTest = mkAssertionOfIO $ do
  runEmulatorInTest $ do
    -- Prepare Frontend CLI actors
    actors@[seller, bidder1, bidder2] <- return [Alice, Bob, Carol]

    runL1InEmulator $ mapM_ (initWallet 200_000_000) actors
    -- liftIO $ putStrLn "Actors initialized"

    -- Init hydra

    headId <- emulateDelegatesStart

    -- TODO: Test state of platform

    -- Create

    terms <-
      runL1InEmulator $
        withActor seller $
          createTermsWithTestNFT config headId
    _ <-
      runL1InEmulator $
        withActor seller $
          announceAndStartBidding terms

    -- Place bid on L1
    bidder1SellerSignature <- liftIO $ sellerSignatureForActor terms bidder1
    runL1InEmulator $ withActor bidder1 $ do
      waitUntil $ biddingStart terms
      newBid terms (correctBidNo terms 0) bidder1SellerSignature

    -- Move and commit

    emulateCommiting headId terms

    -- Placing bid by delegates on L2

    runCompositeForDelegate Second $
      placeNewBidOnL2AndCheck headId terms bidder1 $
        correctBidNo terms 1
    runCompositeForDelegate Third $
      placeNewBidOnL2AndCheck headId terms bidder2 $
        correctBidNo terms 2
    runCompositeForDelegate Second $
      placeNewBidOnL2AndCheck headId terms bidder1 $
        correctBidNo terms 3
    runCompositeForDelegate Main $
      placeNewBidOnL2AndCheck headId terms bidder1 $
        correctBidNo terms 4

    -- Close Head

    emulateClosing headId terms

    -- Place bid after return to L1
    bidder2SellerSignature <- liftIO $ sellerSignatureForActor terms bidder2
    runL1InEmulator $
      withActor bidder2 $
        newBid terms (correctBidNo terms 5) bidder2SellerSignature

    -- Got lot

    runL1InEmulator $ withActor bidder2 $ do
      waitUntil $ biddingEnd terms
      bidderBuys terms
      lift $ assertNFTNumEquals bidder2 1

    -- Delegates got fees

    -- FIXUP: check amount change, this is impossible without fee tracking
    emulateCleanup terms

-- Regression test: commit should not fail when delegate has multiple UTxOs
multipleUtxosToCommitTest :: Assertion
multipleUtxosToCommitTest = mkAssertionOfIO $ do
  runEmulatorInTest $ do
    -- Prepare Frontend CLI actors
    actors@[seller, _bidder1, _bidder2] <- return [Alice, Bob, Carol]
    runL1InEmulator $
      withActor seller $
        mapM_ (initWallet 200_000_000) actors

    -- Ensure that delgates have multiple utxo

    replicateM_ 3 $
      runL1InEmulator $
        withActor seller $
          mapM_ (initWallet 200_000_000) $
            (Map.!) actorsByKind HydraNodeActor

    -- Init hydra

    headId <- emulateDelegatesStart

    -- Create

    terms <-
      runL1InEmulator $
        withActor seller $
          createTermsWithTestNFT config headId
    _ <-
      runL1InEmulator $
        withActor seller $
          announceAndStartBidding terms

    -- Move and commit

    emulateCommiting headId terms

-- Abortion testing

earlyAbort :: Assertion
earlyAbort = mkAssertionOfIO $ do
  runEmulatorInTest $ do
    headId <- emulateDelegatesStart

    runCompositeForDelegate Main $ do
      [_response] <- lift $ abort RequiredHydraRequestFailed
      return ()

    void $
      runCompositeForAllDelegates $
        delegateStepOnExpectedHydraEvent
          Any
          [CurrentDelegateState Updated $ Initialized headId Aborted]

lateAbort :: Assertion
lateAbort = mkAssertionOfIO $ do
  runEmulatorInTest $ do
    headId <- emulateDelegatesStart

    actors@[seller, _bidder1, _bidder2] <- return [Alice, Bob, Carol]
    runL1InEmulator $ mapM_ (initWallet 200_000_000) actors

    -- Create

    terms <-
      runL1InEmulator $
        withActor seller $
          createTermsWithTestNFT config headId
    _ <-
      runL1InEmulator $
        withActor seller $
          announceAndStartBidding terms

    -- Move and commit

    emulateCommiting headId terms

    -- Abort

    runCompositeForDelegate Main $ do
      [_response] <- lift $ abort RequiredHydraRequestFailed
      return ()

    void $
      runCompositeForAllDelegates $
        delegateStepOnExpectedHydraEvent
          Any
          [CurrentDelegateState Updated $ Initialized headId Closed]
