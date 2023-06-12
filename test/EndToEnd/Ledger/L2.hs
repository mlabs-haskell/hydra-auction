module EndToEnd.Ledger.L2 (testSuite) where

-- Prelude import
import HydraAuctionUtils.Prelude

-- Haskell imports

import Data.Map qualified as Map

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- Hydra auction imports

import HydraAuction.Delegate (abort)
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
import HydraAuctionUtils.Fixture (Actor (..), ActorKind (..), actorsByKind)
import HydraAuctionUtils.Hydra.Monad (AwaitedHydraEvent (..))
import HydraAuctionUtils.L1.Runner (
  executeL1RunnerWithNodeAs,
  initWallet,
 )
import HydraAuctionUtils.Monads (waitUntil)

-- Hydra auction test imports
import EndToEnd.HydraUtils (
  EmulatorContext (..),
  EmulatorDelegate (..),
  runCompositeForAllDelegates,
  runCompositeForDelegate,
  runEmulatorInTest,
 )
import EndToEnd.Ledger.L1Steps (
  announceAndStartBidding,
  correctBidNo,
  createTermsWithTestNFT,
 )
import EndToEnd.Ledger.L2Steps
import EndToEnd.Utils (assertNFTNumEquals, mkAssertion)
import EndToEnd.Utils qualified as Utils

testSuite :: TestTree
testSuite =
  testGroup
    "Ledger-L2"
    [ testCase "bidder-buys" bidderBuysTest
    , testCase "multiple-utxos-to-commit" multipleUtxosToCommitTest
    , testCase "early-abort" earlyAbort
    , testCase "late-abort" lateAbort
    ]

config :: AuctionTermsConfig
config =
  Utils.config
    { -- L2 stuff testing test take time after bidding end
      configDiffBiddingEnd = 15
    , configDiffVoucherExpiry = 18
    , configDiffCleanup = 22
    }

-- Includes testing L1 biding before and after L2 moves
-- Inculdes testing of placing bid by same delegate who moved standing bid
bidderBuysTest :: Assertion
bidderBuysTest = mkAssertion $ do
  runEmulatorInTest $ do
    -- Prepare Frontend CLI actors
    MkEmulatorContext {l1Node} <- ask
    let node = l1Node

    actors@[seller, bidder1, bidder2] <- return [Alice, Bob, Carol]

    liftIO $
      executeL1RunnerWithNodeAs node seller $
        mapM_ (initWallet 200_000_000) actors
    liftIO $ putStrLn "Actors initialized"

    -- Init hydra

    headId <- emulateDelegatesStart

    -- Create

    terms <-
      liftIO $
        executeL1RunnerWithNodeAs node seller $
          createTermsWithTestNFT config headId
    _ <-
      liftIO $
        executeL1RunnerWithNodeAs node seller $
          announceAndStartBidding terms

    -- Place bid on L1
    bidder1SellerSignature <- liftIO $ sellerSignatureForActor terms bidder1
    liftIO $
      executeL1RunnerWithNodeAs node bidder1 $ do
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
    liftIO $
      executeL1RunnerWithNodeAs node bidder2 $
        newBid terms (correctBidNo terms 5) bidder2SellerSignature

    -- Got lot

    liftIO $ executeL1RunnerWithNodeAs node bidder2 $ do
      waitUntil $ biddingEnd terms
      bidderBuys terms
      lift $ assertNFTNumEquals bidder2 1

    -- Delegates got fees

    -- FIXUP: check amount change, this is impossible without fee tracking
    emulateCleanup headId terms

-- Regression test: commit should not fail when delegate has multiple UTxOs
multipleUtxosToCommitTest :: Assertion
multipleUtxosToCommitTest = mkAssertion $ do
  runEmulatorInTest $ do
    -- Prepare Frontend CLI actors
    MkEmulatorContext {l1Node} <- ask
    let node = l1Node

    actors@[seller, _bidder1, _bidder2] <- return [Alice, Bob, Carol]
    liftIO $
      executeL1RunnerWithNodeAs node seller $
        mapM_ (initWallet 200_000_000) actors

    -- Ensure that delgates have multiple utxo

    replicateM_ 3 $
      liftIO $
        executeL1RunnerWithNodeAs node seller $
          mapM_ (initWallet 200_000_000) $
            (Map.!) actorsByKind HydraNodeActor

    -- Init hydra

    headId <- emulateDelegatesStart

    -- Create

    terms <-
      liftIO $
        executeL1RunnerWithNodeAs node seller $
          createTermsWithTestNFT config headId
    _ <-
      liftIO $
        executeL1RunnerWithNodeAs node seller $
          announceAndStartBidding terms

    -- Move and commit

    emulateCommiting headId terms

-- Abortion testing

earlyAbort :: Assertion
earlyAbort = mkAssertion $ do
  runEmulatorInTest $ do
    headId <- emulateDelegatesStart

    runCompositeForDelegate Main $ do
      [_response] <- abort RequiredHydraRequestFailed
      return ()

    void $
      runCompositeForAllDelegates $
        delegateStepOnExpectedHydraEvent
          Any
          [CurrentDelegateState Updated $ Initialized headId Aborted]

lateAbort :: Assertion
lateAbort = mkAssertion $ do
  runEmulatorInTest $ do
    MkEmulatorContext {l1Node} <- ask

    headId <- emulateDelegatesStart

    actors@[seller, _bidder1, _bidder2] <- return [Alice, Bob, Carol]
    liftIO $
      executeL1RunnerWithNodeAs l1Node seller $
        mapM_ (initWallet 200_000_000) actors

    -- Create

    terms <-
      liftIO $
        executeL1RunnerWithNodeAs l1Node seller $
          createTermsWithTestNFT config headId
    _ <-
      liftIO $
        executeL1RunnerWithNodeAs l1Node seller $
          announceAndStartBidding terms

    -- Move and commit

    emulateCommiting headId terms

    -- Abort

    runCompositeForDelegate Main $ do
      [_response] <- abort RequiredHydraRequestFailed
      return ()

    void $
      runCompositeForAllDelegates $
        delegateStepOnExpectedHydraEvent
          Any
          [CurrentDelegateState Updated $ Initialized headId Closed]
