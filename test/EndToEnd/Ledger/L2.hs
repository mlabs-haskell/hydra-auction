module EndToEnd.Ledger.L2 (testSuite) where

-- Prelude import
import Prelude

-- Haskell imports
import Control.Monad (replicateM_)
import Control.Monad.Trans (MonadIO (..))

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- Hydra imports
import Hydra.Prelude (ask)

-- Hydra auction imports
import HydraAuction.Runner (
  executeRunnerWithNodeAs,
  initWallet,
 )
import HydraAuction.Runner.Time (waitUntil)
import HydraAuction.Tx.Escrow (
  bidderBuys,
 )
import HydraAuction.Tx.StandingBid (newBid)
import HydraAuction.Tx.TermsConfig (AuctionTermsConfig (..))
import HydraAuction.Types (AuctionTerms (..))
import HydraAuctionUtils.Fixture (Actor (..), hydraNodeActors)

-- Hydra auction test imports
import EndToEnd.HydraUtils (
  EmulatorContext (..),
  EmulatorDelegate (..),
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
bidderBuysTest :: Assertion
bidderBuysTest = mkAssertion $ do
  runEmulatorInTest $ do
    -- Prepare Frontend CLI actors
    MkEmulatorContext {l1Node} <- ask
    let node = l1Node

    actors@[seller, bidder1, bidder2] <- return [Alice, Bob, Carol]

    liftIO $
      executeRunnerWithNodeAs node seller $
        mapM_ (initWallet 200_000_000) actors
    liftIO $ putStrLn "Actors initialized"

    -- Init hydra

    headId <- emulateDelegatesStart

    -- Create

    terms <-
      liftIO $
        executeRunnerWithNodeAs node seller $
          createTermsWithTestNFT config headId
    _ <-
      liftIO $
        executeRunnerWithNodeAs node seller $
          announceAndStartBidding terms

    -- Place bid on L1

    liftIO $
      executeRunnerWithNodeAs node bidder1 $
        newBid terms $
          correctBidNo terms 0

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

    -- Close Head

    emulateClosing headId

    -- Place bid after return to L1

    liftIO $
      executeRunnerWithNodeAs node bidder2 $
        newBid terms $
          correctBidNo terms 4

    -- Got lot

    liftIO $ executeRunnerWithNodeAs node bidder2 $ do
      waitUntil $ biddingEnd terms
      bidderBuys terms
      assertNFTNumEquals bidder2 1

-- Regression test: commit should not fail when delegate has multiple UTxOs
multipleUtxosToCommitTest :: Assertion
multipleUtxosToCommitTest = mkAssertion $ do
  runEmulatorInTest $ do
    -- Prepare Frontend CLI actors
    MkEmulatorContext {l1Node} <- ask
    let node = l1Node

    actors@[seller, _bidder1, _bidder2] <- return [Alice, Bob, Carol]
    liftIO $
      executeRunnerWithNodeAs node seller $
        mapM_ (initWallet 200_000_000) actors

    -- Ensure that delgates have multiple utxo

    replicateM_ 3 $
      liftIO $
        executeRunnerWithNodeAs node seller $
          mapM_ (initWallet 200_000_000) hydraNodeActors

    -- Init hydra

    headId <- emulateDelegatesStart

    -- Create

    terms <-
      liftIO $
        executeRunnerWithNodeAs node seller $
          createTermsWithTestNFT config headId
    _ <-
      liftIO $
        executeRunnerWithNodeAs node seller $
          announceAndStartBidding terms

    -- Move and commit

    emulateCommiting headId terms
