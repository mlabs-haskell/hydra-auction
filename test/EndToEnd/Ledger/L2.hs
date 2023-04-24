module EndToEnd.Ledger.L2 (testSuite) where

-- Prelude import
import PlutusTx.Prelude ((*), (+))
import Prelude hiding ((*), (+))

-- Haskell imports

import Control.Monad (forM_)
import Control.Monad.Trans (MonadIO (..), MonadTrans (lift))
import Data.Maybe (fromJust)

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports

import Hydra.Cardano.Api (mkTxIn)
import Hydra.Chain.Direct.Tx (headIdToCurrencySymbol)
import Hydra.Prelude (ask)

-- Hydra auction imports

import HydraAuction.Delegate (
  ClientResponseScope (..),
  DelegateEvent (..),
  delegateEventStep,
  delegateFrontendRequestStep,
 )
import HydraAuction.Delegate.Interface (
  DelegateResponse (..),
  DelegateState (..),
  FrontendRequest (..),
  InitializedState (..),
  ResponseReason (..),
 )
import HydraAuction.Hydra.Interface (HydraEvent (..), HydraEventKind (..))
import HydraAuction.Hydra.Monad (
  AwaitedHydraEvent (..),
  waitForHydraEvent,
 )
import HydraAuction.HydraHacks (prepareScriptRegistry)
import HydraAuction.Runner (
  ExecutionContext (MkExecutionContext, node),
  executeRunnerWithNodeAs,
  initWallet,
  withActor,
 )
import HydraAuction.Runner.Time (waitUntil)
import HydraAuction.Tx.Common (scriptUtxos)
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  startBidding,
 )
import HydraAuction.Tx.TermsConfig (
  AuctionTermsConfig (
    AuctionTermsConfig,
    configAuctionFeePerDelegate,
    configDiffBiddingEnd,
    configDiffBiddingStart,
    configDiffCleanup,
    configDiffVoucherExpiry,
    configMinimumBidIncrement,
    configStartingBid
  ),
  configToAuctionTerms,
  constructTermsDynamic,
 )
import HydraAuction.Tx.TestNFT (mintOneTestNFT)
import HydraAuction.Types (
  ApprovedBidders (..),
  AuctionStage (..),
  AuctionTerms (..),
  StandingBidDatum (standingBidState),
  intToNatural,
  standingBid,
 )
import HydraAuctionUtils.Fixture (Actor (..), getActorsPubKeyHash, keysFor)

-- Hydra auction test imports

import EndToEnd.HydraUtils (
  EmulatorDelegate (..),
  runCompositeForAllDelegates,
  runCompositeForDelegate,
  runEmulator,
  spinUpHeads,
 )
import EndToEnd.Utils (assertNFTNumEquals, mkAssertion)
import HydraAuction.Delegate.CompositeRunner (runHydraInComposite)
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Tx.StandingBid (createStandingBidDatum)

testSuite :: TestTree
testSuite =
  testGroup
    "Ledger-L2"
    [testCase "bidder-buys" bidderBuysTest]

config :: AuctionTermsConfig
config =
  AuctionTermsConfig
    { configDiffBiddingStart = 2
    , configDiffBiddingEnd = 5
    , -- L2 stuff testing test take time after bidding end
      configDiffVoucherExpiry = 15
    , configDiffCleanup = 20
    , configAuctionFeePerDelegate = fromJust $ intToNatural 4_000_000
    , configStartingBid = fromJust $ intToNatural 8_000_000
    , configMinimumBidIncrement = fromJust $ intToNatural 8_000_000
    }

bidderBuysTest :: Assertion
bidderBuysTest = mkAssertion $ do
  MkExecutionContext {node} <- ask
  -- FIXME: should use already deployed registry as real code would
  (hydraScriptsTxId, scriptRegistry) <- liftIO $ prepareScriptRegistry node
  liftIO $ putStrLn "prepareScriptRegistry called"

  spinUpHeads 0 hydraScriptsTxId $ \clients -> runEmulator clients $ do
    -- Prepare Frontend CLI actors

    let actors@[seller, bidder1, bidder2] = [Alice, Bob, Carol]

    liftIO $
      executeRunnerWithNodeAs node seller $
        mapM_ (initWallet 200_000_000) actors
    liftIO $ putStrLn "Actors initialized"

    -- Init hydra

    runCompositeForDelegate node Main $ do
      [] <- delegateEventStep Start
      return ()

    headId : _ <- runCompositeForAllDelegates node $ do
      event@(HeadIsInitializing headId) <- waitForHydraEvent Any
      responses <- delegateEventStep $ HydraEvent event
      liftIO $
        assertEqual
          "Initializing reaction"
          responses
          [CurrentDelegateState Updated $ Initialized headId NotYetOpen]
      return headId

    -- Create

    utxoRef <- liftIO $ executeRunnerWithNodeAs node seller $ do
      nftTx <- mintOneTestNFT
      return $ mkTxIn nftTx 0

    terms <- liftIO $ do
      dynamicState <-
        constructTermsDynamic seller utxoRef (headIdToCurrencySymbol headId)
      configToAuctionTerms config dynamicState

    [(standingBidTxIn, _)] <-
      liftIO $
        executeRunnerWithNodeAs node seller $ do
          announceAuction terms

          waitUntil $ biddingStart terms

          -- FIXME: checks are disabled until M5
          actorsPkh <- liftIO $ getActorsPubKeyHash []
          startBidding terms (ApprovedBidders actorsPkh)

          UTxO.pairs <$> scriptUtxos StandingBid terms

    -- Move and commit

    runCompositeForDelegate node Main $ do
      responses <-
        delegateFrontendRequestStep
          ( 1
          , CommitStandingBid
              { auctionTerms = terms
              , utxoToCommit = standingBidTxIn
              }
          )
      liftIO $
        assertEqual "Commit Main" responses [(Broadcast, AuctionSet terms)]

    forM_ [Second, Third] $ flip (runCompositeForDelegate node) $ do
      delegateStepOnHydraEvent
        (SpecificKind CommittedKind)
        [CurrentDelegateState Updated (Initialized headId HasCommit)]

    _ <-
      runCompositeForAllDelegates node $
        delegateStepOnHydraEvent
          (SpecificKind HeadIsOpenKind)
          [CurrentDelegateState Updated (Initialized headId $ Open Nothing)]

    -- Placing bid by delegates

    placeNewBidAndCheck node headId terms Second bidder1 $ startingBid terms
    placeNewBidAndCheck node headId terms Third bidder2 $
      startingBid terms + minimumBidIncrement terms
    placeNewBidAndCheck node headId terms Second bidder1 $
      startingBid terms
        + fromJust (intToNatural 2) * minimumBidIncrement terms

    -- Close Head

    liftIO $ executeRunnerWithNodeAs node seller $ waitUntil $ biddingEnd terms
    [] <-
      runCompositeForDelegate node Main $
        delegateEventStep $
          AuctionStageStarted BiddingEndedStage

    _ <-
      runCompositeForAllDelegates node $
        delegateStepOnHydraEvent
          (SpecificKind HeadIsClosedKind)
          [CurrentDelegateState Updated (Initialized headId Closed)]
    _ <-
      runCompositeForAllDelegates node $
        delegateStepOnHydraEvent
          (SpecificKind ReadyToFanoutKind)
          []
    _ <-
      runCompositeForAllDelegates node $
        delegateStepOnHydraEvent
          (SpecificKind HeadIsFinalizedKind)
          [CurrentDelegateState Updated (Initialized headId Finalized)]

    -- Got lot

    liftIO $ executeRunnerWithNodeAs node bidder1 $ do
      bidderBuys terms
      assertNFTNumEquals bidder1 1
  where
    placeNewBidAndCheck node headId terms delegate bidder amount =
      runCompositeForDelegate node delegate $ do
        let clientId = fromEnum delegate
        (bidderPublicKey, _) <- liftIO $ keysFor bidder

        -- Place new bid
        let bidDatum = createStandingBidDatum terms amount bidderPublicKey
        responses <-
          delegateFrontendRequestStep
            (clientId, NewBid {auctionTerms = terms, datum = bidDatum})
        liftIO $
          assertEqual
            "New bid"
            responses
            [(PerClient clientId, ClosingTxTemplate)]

        -- Check snapshot
        let expectedBidTerms = standingBid $ standingBidState bidDatum
        delegateStepOnHydraEvent
          (SpecificKind SnapshotConfirmedKind)
          [ CurrentDelegateState
              Updated
              (Initialized headId $ Open expectedBidTerms)
          ]
    delegateStepOnHydraEvent eventSpec expectedResponses = do
      event <-
        lift $
          runHydraInComposite $
            waitForHydraEvent eventSpec
      responses <- delegateEventStep $ HydraEvent event
      let message = "HydraEvent delegate reaction on " <> show eventSpec
      liftIO $ assertEqual message responses expectedResponses
