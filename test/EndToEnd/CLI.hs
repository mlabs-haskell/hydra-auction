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
import HydraAuction.Platform.Storage (EntityStorage (..), initialStorage, processClientInput)
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

handleCliActionWithMocks :: IORef EntityStorage -> CliAction -> L1Runner ()
handleCliActionWithMocks platformSRef action = do
  delegateS <- newIORef mockDelegateState
  platformS <- readIORef platformSRef
  handleCliAction
    (\_ -> pure ())
    ( \platformReq -> do
        let (response, newState) = runState (processClientInput platformReq) platformS
        writeIORef platformSRef newState
        pure response
    )
    delegateS
    action

bidderBuysTest :: Assertion
bidderBuysTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  platformS <- newIORef initialStorage

  handleCliActionWithMocks platformS $ Prepare seller

  assertNFTNumEquals seller 1

  handleCliActionWithMocks platformS $ AuctionAnounce auctionName

  terms <- auctionTermsFor auctionName

  handleCliActionWithMocks platformS $ SubmitSignatureToPlatform auctionName buyer1
  handleCliActionWithMocks platformS $ SubmitSignatureToPlatform auctionName buyer2

  waitUntil $ biddingStart terms

  handleCliActionWithMocks platformS $ StartBidding auctionName

  assertNFTNumEquals seller 0

  withActor buyer2 $ handleCliActionWithMocks platformS $ NewBid auctionName (startingBid terms) L1

  waitUntil $ biddingEnd terms

  withActor buyer2 $ handleCliActionWithMocks platformS $ BidderBuys auctionName

  assertNFTNumEquals seller 0
  assertNFTNumEquals buyer1 0
  assertNFTNumEquals buyer2 1

  waitUntil $ cleanup terms
  handleCliActionWithMocks platformS $ Cleanup auctionName

-- Test reclaim with BidderClaimsDeposit and using deposit for BidderBuys
depositTest :: Assertion
depositTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  platformS <- newIORef initialStorage

  handleCliActionWithMocks platformS $ Prepare seller

  assertNFTNumEquals seller 1

  handleCliActionWithMocks platformS $ AuctionAnounce auctionName

  terms <- auctionTermsFor auctionName

  withActor buyer1 $ handleCliActionWithMocks platformS $ MakeDeposit auctionName (fromJust $ intToNatural 10_000_000)
  withActor buyer2 $ handleCliActionWithMocks platformS $ MakeDeposit auctionName (fromJust $ intToNatural 10_000_000)

  assertUTxOsInScriptEquals Deposit terms 2

  handleCliActionWithMocks platformS $ SubmitSignatureToPlatform auctionName buyer1
  handleCliActionWithMocks platformS $ SubmitSignatureToPlatform auctionName buyer2

  waitUntil $ biddingStart terms

  handleCliActionWithMocks platformS $ StartBidding auctionName

  assertNFTNumEquals seller 0

  withActor buyer2 $ handleCliActionWithMocks platformS $ NewBid auctionName (startingBid terms) L1

  waitUntil $ biddingEnd terms

  withActor buyer2 $ handleCliActionWithMocks platformS $ BidderBuys auctionName

  assertNFTNumEquals seller 0
  assertNFTNumEquals buyer1 0
  assertNFTNumEquals buyer2 1

  assertUTxOsInScriptEquals Deposit terms 1

  withActor buyer1 $ handleCliActionWithMocks platformS $ BidderClaimsDeposit auctionName

  assertUTxOsInScriptEquals Deposit terms 0

  waitUntil $ cleanup terms
  handleCliActionWithMocks platformS $ Cleanup auctionName

-- Test case of reclaiming
depositCleanupClaimTest :: Assertion
depositCleanupClaimTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob

  platformS <- newIORef initialStorage

  handleCliActionWithMocks platformS $ Prepare seller

  assertNFTNumEquals seller 1

  handleCliActionWithMocks platformS $ AuctionAnounce auctionName

  terms <- auctionTermsFor auctionName

  withActor buyer1 $
    handleCliActionWithMocks platformS $
      MakeDeposit auctionName (fromJust $ intToNatural 10_000_000)
  assertUTxOsInScriptEquals Deposit terms 1

  waitUntil $ biddingStart terms

  handleCliActionWithMocks platformS $ SubmitSignatureToPlatform auctionName buyer1

  handleCliActionWithMocks platformS $ StartBidding auctionName

  withActor buyer1 $
    handleCliActionWithMocks platformS $
      NewBid auctionName (startingBid terms) L1

  waitUntil $ biddingEnd terms

  waitUntil $ cleanup terms

  withActor buyer1 $
    handleCliActionWithMocks platformS $
      CleanupDeposit auctionName

  assertUTxOsInScriptEquals Deposit terms 0
