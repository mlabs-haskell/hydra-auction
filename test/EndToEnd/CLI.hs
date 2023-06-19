module EndToEnd.CLI (testSuite) where

-- Prelude imports
import Hydra.Prelude

-- Haskell imports

import Data.Maybe (fromJust)

-- Haskell test imports

import System.Directory (removeFile)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, testCase)

-- Hydra auction imports

import HydraAuction.Delegate.Interface (
  DelegateState (..),
  InitializedState (..),
  OpenHeadUtxo (..),
 )
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Tx.TermsConfig (nonExistentHeadIdStub)
import HydraAuction.Types (AuctionTerms (..))
import HydraAuctionUtils.Fixture (Actor (..))
import HydraAuctionUtils.L1.Runner (
  L1Runner,
  withActor,
 )
import HydraAuctionUtils.Monads (waitUntil)
import HydraAuctionUtils.Monads.Actors (WithActorT)
import HydraAuctionUtils.Types.Natural (intToNatural)

-- CLI imports

import CLI.Actions (Layer (..), auctionTermsFor, handleCliAction)
import CLI.Config (AuctionName, DirectoryKind (..), toJsonFileName, writeAuctionTermsConfig)
import CLI.Types (CliAction (..), PerAuctionCliAction (..))

-- Hydra auction test imports
import EndToEnd.Utils (assertNFTNumEquals, assertUTxOsInScriptEquals, config, mkAssertion)

testSuite :: TestTree
testSuite =
  withResource createTestConfig deleteTestConfig $
    const $
      testGroup
        "L1 - CLI"
        [ testCase "bidder-buys" bidderBuysTest
        , testCase "deposit-test" depositTest
        , testCase "deposit-cleanup-claim" depositCleanupClaimTest
        ]
  where
    createTestConfig = writeAuctionTermsConfig auctionName config
    deleteTestConfig = const $ do
      fn <- toJsonFileName AuctionConfig auctionName
      removeFile fn

mockDelegateState :: DelegateState
mockDelegateState =
  Initialized nonExistentHeadIdStub $
    Open headUtxo Nothing
  where
    headUtxo =
      MkOpenHeadUtxo
        { standingBidTerms = Nothing
        , standingBidUtxo = (error "unused", error "unused")
        , collateralUtxo = (error "unused", error "unused")
        }

auctionName :: AuctionName
auctionName = "test"

handleCliActionWithMockDelegates :: CliAction -> WithActorT L1Runner ()
handleCliActionWithMockDelegates action = do
  delegateS <- newIORef mockDelegateState
  handleCliAction (\_ -> pure ()) delegateS action

bidderBuysTest :: Assertion
bidderBuysTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  withActor seller $ handleCliActionWithMockDelegates $ Prepare seller

  assertNFTNumEquals seller 1

  withActor seller $ handleCliActionWithMockDelegates $ PerAuction auctionName AuctionAnounce

  terms <- auctionTermsFor auctionName

  waitUntil $ biddingStart terms

  withActor seller $ handleCliActionWithMockDelegates $ PerAuction auctionName StartBidding

  assertNFTNumEquals seller 0

  withActor buyer2 $ handleCliActionWithMockDelegates $ PerAuction auctionName $ NewBid (startingBid terms) L1

  waitUntil $ biddingEnd terms

  withActor buyer2 $ handleCliActionWithMockDelegates $ PerAuction auctionName BidderBuys

  assertNFTNumEquals seller 0
  assertNFTNumEquals buyer1 0
  assertNFTNumEquals buyer2 1

  waitUntil $ cleanup terms
  withActor seller $ handleCliActionWithMockDelegates $ PerAuction auctionName Cleanup

-- Test reclaim with BidderClaimsDeposit and using deposit for BidderBuys
depositTest :: Assertion
depositTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  withActor seller $ handleCliActionWithMockDelegates $ Prepare seller

  assertNFTNumEquals seller 1

  withActor seller $ handleCliActionWithMockDelegates $ PerAuction auctionName AuctionAnounce

  terms <- auctionTermsFor auctionName

  withActor buyer1 $ handleCliActionWithMockDelegates $ PerAuction auctionName $ MakeDeposit (fromJust $ intToNatural 10_000_000)
  withActor buyer2 $ handleCliActionWithMockDelegates $ PerAuction auctionName $ MakeDeposit (fromJust $ intToNatural 10_000_000)

  assertUTxOsInScriptEquals Deposit terms 2

  waitUntil $ biddingStart terms

  withActor seller $ handleCliActionWithMockDelegates $ PerAuction auctionName StartBidding

  assertNFTNumEquals seller 0

  withActor buyer2 $ handleCliActionWithMockDelegates $ PerAuction auctionName $ NewBid (startingBid terms) L1

  waitUntil $ biddingEnd terms

  withActor buyer2 $ handleCliActionWithMockDelegates $ PerAuction auctionName BidderBuys

  assertNFTNumEquals seller 0
  assertNFTNumEquals buyer1 0
  assertNFTNumEquals buyer2 1

  assertUTxOsInScriptEquals Deposit terms 1

  withActor buyer1 $ handleCliActionWithMockDelegates $ PerAuction auctionName BidderClaimsDeposit

  assertUTxOsInScriptEquals Deposit terms 0

  waitUntil $ cleanup terms
  withActor seller $ handleCliActionWithMockDelegates $ PerAuction auctionName Cleanup

-- Test case of reclaiming
depositCleanupClaimTest :: Assertion
depositCleanupClaimTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob

  withActor seller $ handleCliActionWithMockDelegates $ Prepare seller

  assertNFTNumEquals seller 1

  withActor seller $ handleCliActionWithMockDelegates $ PerAuction auctionName AuctionAnounce

  terms <- auctionTermsFor auctionName

  withActor buyer1 $
    handleCliActionWithMockDelegates $
      PerAuction auctionName $
        MakeDeposit (fromJust $ intToNatural 10_000_000)
  assertUTxOsInScriptEquals Deposit terms 1

  waitUntil $ biddingStart terms

  withActor seller $ handleCliActionWithMockDelegates $ PerAuction auctionName StartBidding

  withActor buyer1 $
    handleCliActionWithMockDelegates $
      PerAuction auctionName $
        NewBid (startingBid terms) L1

  waitUntil $ biddingEnd terms

  waitUntil $ cleanup terms

  withActor buyer1 $
    handleCliActionWithMockDelegates $
      PerAuction auctionName CleanupDeposit
  assertUTxOsInScriptEquals Deposit terms 0
