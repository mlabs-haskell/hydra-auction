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

import HydraAuction.Delegate.Interface (DelegateState (..), InitializedState (..))
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Tx.TermsConfig (nonExistentHeadIdStub)
import HydraAuction.Types (AuctionTerms (..))
import HydraAuctionUtils.Fixture (Actor (..))
import HydraAuctionUtils.L1.Runner (
  L1Runner,
  withActor,
 )
import HydraAuctionUtils.L1.Runner.Time (waitUntil)
import HydraAuctionUtils.Types.Natural (intToNatural)

-- CLI imports

import CLI.Actions (CliAction (..), Layer (..), auctionTermsFor, handleCliAction)
import CLI.Config (AuctionName, DirectoryKind (..), toJsonFileName, writeAuctionTermsConfig)

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
        ]
  where
    createTestConfig = writeAuctionTermsConfig auctionName config
    deleteTestConfig = const $ do
      fn <- toJsonFileName AuctionConfig auctionName
      removeFile fn

mockDelegateState :: DelegateState
mockDelegateState = Initialized nonExistentHeadIdStub (Open Nothing)

auctionName :: AuctionName
auctionName = "test"

handleCliActionWithMockDelegates :: CliAction -> L1Runner ()
handleCliActionWithMockDelegates action = do
  delegateS <- newIORef mockDelegateState
  handleCliAction (\_ -> pure ()) delegateS action

bidderBuysTest :: Assertion
bidderBuysTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  handleCliActionWithMockDelegates $ Prepare seller

  assertNFTNumEquals seller 1

  handleCliActionWithMockDelegates $ AuctionAnounce auctionName

  terms <- auctionTermsFor auctionName

  waitUntil $ biddingStart terms

  handleCliActionWithMockDelegates $ StartBidding auctionName [buyer1, buyer2]

  assertNFTNumEquals seller 0

  withActor buyer2 $ handleCliActionWithMockDelegates $ NewBid auctionName (startingBid terms) L1

  waitUntil $ biddingEnd terms

  withActor buyer2 $ handleCliActionWithMockDelegates $ BidderBuys auctionName

  assertNFTNumEquals seller 0
  assertNFTNumEquals buyer1 0
  assertNFTNumEquals buyer2 1

  waitUntil $ cleanup terms
  handleCliActionWithMockDelegates $ Cleanup auctionName

depositTest :: Assertion
depositTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  handleCliActionWithMockDelegates $ Prepare seller

  assertNFTNumEquals seller 1

  handleCliActionWithMockDelegates $ AuctionAnounce auctionName

  terms <- auctionTermsFor auctionName

  withActor buyer1 $ handleCliActionWithMockDelegates $ MakeDeposit auctionName (fromJust $ intToNatural 10_000_000)
  withActor buyer2 $ handleCliActionWithMockDelegates $ MakeDeposit auctionName (fromJust $ intToNatural 10_000_000)

  assertUTxOsInScriptEquals Deposit terms 2

  waitUntil $ biddingStart terms

  handleCliActionWithMockDelegates $ StartBidding auctionName [buyer1, buyer2]

  assertNFTNumEquals seller 0

  withActor buyer2 $ handleCliActionWithMockDelegates $ NewBid auctionName (startingBid terms) L1

  waitUntil $ biddingEnd terms

  withActor buyer2 $ handleCliActionWithMockDelegates $ BidderBuys auctionName

  assertNFTNumEquals seller 0
  assertNFTNumEquals buyer1 0
  assertNFTNumEquals buyer2 1

  assertUTxOsInScriptEquals Deposit terms 1

  withActor buyer1 $ handleCliActionWithMockDelegates $ BidderClaimsDeposit auctionName

  assertUTxOsInScriptEquals Deposit terms 0

  waitUntil $ cleanup terms
  handleCliActionWithMockDelegates $ Cleanup auctionName
