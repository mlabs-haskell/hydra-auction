module EndToEnd.CLI (testSuite) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports

import Data.IORef (newIORef)

-- Haskell test imports

import System.Directory (removeFile)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, testCase)

-- Hydra imports
import Hydra.Cardano.Api (Lovelace (..))

-- Hydra auction imports

import HydraAuction.Delegate.Interface (
  DelegateProtocol,
  DelegateState (..),
  InitializedState (..),
  OpenHeadUtxo (..),
  OpenState (..),
 )
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Platform.Interface (PlatformProtocol)
import HydraAuction.Platform.Storage (PlatformImplementation)
import HydraAuction.Tx.TermsConfig (nonExistentHeadIdStub)
import HydraAuction.Types (AuctionTerms (..))
import HydraAuctionUtils.Fixture (Actor (..))
import HydraAuctionUtils.L1.Runner (
  L1Runner,
  withActor,
 )
import HydraAuctionUtils.Monads (waitUntil)
import HydraAuctionUtils.Monads.Actors (WithActorT)

import HydraAuctionUtils.Server.Client (
  FakeProtocolClient,
  ProtocolClientFor,
  newFakeClient,
 )

-- CLI imports

import CLI.Actions (
  CliActionHandle (..),
  Layer (..),
  auctionTermsFor,
  handleCliAction,
 )
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

mockDelegateState :: DelegateState DelegateProtocol
mockDelegateState =
  Initialized nonExistentHeadIdStub $
    Open $
      MkOpenState headUtxo Nothing
  where
    headUtxo =
      MkOpenHeadUtxo
        { standingBidTerms = Nothing
        , standingBidUtxo = (error "unused", error "unused")
        , collateralUtxo = (error "unused", error "unused")
        }

auctionName :: AuctionName
auctionName = "test"

newFakeHandle ::
  forall m.
  (MonadIO m) =>
  m (CliActionHandle (FakeProtocolClient PlatformImplementation))
newFakeHandle = do
  currentDelegateStateRef <- liftIO $ newIORef mockDelegateState
  platformClient <- liftIO newFakeClient
  return $
    MkCliActionHandle
      { platformClient
      , sendRequestToDelegate = \_ -> pure ()
      , currentDelegateStateRef
      }

performAnnounceAndApproveBidders ::
  forall client.
  ProtocolClientFor PlatformProtocol client =>
  CliActionHandle client ->
  [Actor] ->
  WithActorT L1Runner ()
performAnnounceAndApproveBidders handle approvedBidders = do
  handleCliAction handle $ PerAuction auctionName AuctionAnounce
  forM_ approvedBidders $ \bidder ->
    handleCliAction handle $
      PerAuction auctionName $
        ApproveBidder bidder

bidderBuysTest :: Assertion
bidderBuysTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  handle <- newFakeHandle
  let handleCliActionWithMockDelegates = handleCliAction handle

  withActor seller $ handleCliActionWithMockDelegates $ Prepare seller

  assertNFTNumEquals seller 1

  withActor seller $ performAnnounceAndApproveBidders handle [buyer1, buyer2]

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
  handle <- newFakeHandle
  let handleCliActionWithMockDelegates = handleCliAction handle

  withActor seller $ handleCliActionWithMockDelegates $ Prepare seller

  assertNFTNumEquals seller 1

  withActor seller $ performAnnounceAndApproveBidders handle [buyer1, buyer2]

  terms <- auctionTermsFor auctionName

  withActor buyer1 $
    handleCliActionWithMockDelegates $
      PerAuction auctionName $
        MakeDeposit (Lovelace 10_000_000)
  withActor buyer2 $
    handleCliActionWithMockDelegates $
      PerAuction auctionName $
        MakeDeposit (Lovelace 10_000_000)

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
  handle <- newFakeHandle
  let handleCliActionWithMockDelegates = handleCliAction handle

  withActor seller $ handleCliActionWithMockDelegates $ Prepare seller

  assertNFTNumEquals seller 1

  withActor seller $ performAnnounceAndApproveBidders handle [buyer1]

  terms <- auctionTermsFor auctionName

  withActor buyer1 $
    handleCliActionWithMockDelegates $
      PerAuction auctionName $
        MakeDeposit (Lovelace 10_000_000)
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
