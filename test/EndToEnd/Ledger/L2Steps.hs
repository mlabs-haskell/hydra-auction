module EndToEnd.Ledger.L2Steps (
  delegateStepOnExpectedHydraEvent,
  emulateDelegatesStart,
  emulateClosing,
  emulateCleanup,
  emulateCommiting,
  placeNewBidOnL2AndCheck,
  dropHydraEvent,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell test imports
import Test.Tasty.HUnit (assertBool, assertEqual)

-- Hydra imports

import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Chain (HeadId)

-- HydraAuction imports

import HydraAuction.Delegate (
  DelegateEvent (..),
  delegateFrontendRequestStep,
 )
import HydraAuction.Delegate.Interface (
  DelegateResponse (..),
  DelegateState (..),
  FrontendRequest (..),
  InitializedState (..),
  OpenHeadUtxo (..),
  ResponseReason (..),
 )
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Tx.Common (scriptSingleUtxo)
import HydraAuction.Tx.StandingBid (createStandingBidDatum, sellerSignatureForActor)
import HydraAuction.Types (
  AuctionStage (..),
  AuctionTerms,
  BidTerms,
  StandingBidDatum,
  standingBid,
  standingBidState,
 )
import HydraAuctionUtils.Composite.Runner (CompositeRunner, runL1RunnerInComposite)
import HydraAuctionUtils.Fixture (Actor, keysFor)
import HydraAuctionUtils.Hydra.Interface (HydraEvent, HydraEventKind (..))
import HydraAuctionUtils.Hydra.Monad (
  AwaitedHydraEvent (..),
  waitForHydraEvent,
  waitForHydraEvent',
 )
import HydraAuctionUtils.Monads.Actors (MonadHasActor (..))
import HydraAuctionUtils.Server.ClientId (ClientId, ClientResponseScope (..))
import HydraAuctionUtils.Types.Natural (Natural)

-- HydraAuction test imports
import EndToEnd.HydraUtils (
  DelegatesClusterEmulator,
  EmulatorDelegate (..),
  delegateEventStepEmulated,
  runCompositeForAllDelegates,
  runCompositeForDelegate,
 )

delegateStepOnExpectedHydraEvent' ::
  HasCallStack =>
  AwaitedHydraEvent ->
  ([DelegateResponse] -> Bool) ->
  StateT DelegateState CompositeRunner HydraEvent
delegateStepOnExpectedHydraEvent' eventSpec checkResponses = do
  Just event <- lift $ waitForHydraEvent eventSpec
  delegate <- askActor
  responses <- delegateEventStepEmulated $ HydraEvent event
  let message =
        "HydraEvent delegate "
          <> show delegate
          <> " reaction was "
          <> show responses
          <> " awaited for "
          <> show eventSpec
  liftIO $ assertBool message (checkResponses responses)
  return event

delegateStepOnExpectedHydraEvent ::
  HasCallStack =>
  AwaitedHydraEvent ->
  [DelegateResponse] ->
  StateT DelegateState CompositeRunner HydraEvent
delegateStepOnExpectedHydraEvent eventSpec expectedResponses =
  delegateStepOnExpectedHydraEvent'
    eventSpec
    (expectedResponses ==)

dropHydraEvent :: CompositeRunner ()
dropHydraEvent = void $ do
  r <- waitForHydraEvent' 0 Any
  case r of
    Just _ -> dropHydraEvent
    Nothing -> return ()

emulateDelegatesStart :: HasCallStack => DelegatesClusterEmulator HeadId
emulateDelegatesStart = do
  runCompositeForDelegate Main $ do
    [] <- delegateEventStepEmulated Start
    return ()

  headId : _ <- runCompositeForAllDelegates $ do
    Just event@(HeadIsInitializing {headId}) <- waitForHydraEvent Any
    responses <- delegateEventStepEmulated $ HydraEvent event
    let expectedState =
          Initialized headId $
            AwaitingCommits {stangingBidWasCommited = False}
    liftIO $
      assertEqual
        "Initializing reaction"
        [CurrentDelegateState Updated expectedState]
        responses
    return headId
  return headId

emulateCommiting :: HasCallStack => HeadId -> AuctionTerms -> DelegatesClusterEmulator ()
emulateCommiting headId terms = do
  -- Main delegate commits Standing Bid
  runCompositeForDelegate Main $ do
    Just standingBidSingleUtxo <-
      lift $
        runL1RunnerInComposite $
          scriptSingleUtxo StandingBid terms
    responses <-
      delegateFrontendRequestStep
        ( 1
        , CommitStandingBid
            { auctionTerms = terms
            , utxoToCommit = standingBidSingleUtxo
            }
        )
    liftIO $
      assertEqual "Commit Main" [(Broadcast, AuctionSet terms)] responses

  -- Secondary delegates should commit money on L2 in reaction
  -- (we do not check that)
  -- They states should reflect existence of first commit now
  let expectedState =
        Initialized headId $
          AwaitingCommits {stangingBidWasCommited = True}
  _ <-
    runCompositeForAllDelegates $ do
      delegateStepOnExpectedHydraEvent
        (SpecificKind CommittedKind)
        [CurrentDelegateState Updated expectedState]

  -- Secondary commits creates Commited events, which Delegates should ignore

  replicateM_ 2 $
    runCompositeForAllDelegates $
      delegateStepOnExpectedHydraEvent
        (SpecificKind CommittedKind)
        []

  -- After all Hydra got open and states of every Delegate reflect that

  _ <-
    runCompositeForAllDelegates $
      delegateStepOnExpectedHydraEvent'
        (SpecificKind HeadIsOpenKind)
        (const True)

  return ()

emulateClosing ::
  HasCallStack => HeadId -> AuctionTerms -> DelegatesClusterEmulator ()
emulateClosing headId terms = do
  [] <-
    runCompositeForDelegate Main $
      delegateEventStepEmulated $
        AuctionStageStarted terms BiddingEndedStage
  _ <-
    runCompositeForAllDelegates $
      delegateStepOnExpectedHydraEvent
        (SpecificKind HeadIsClosedKind)
        [CurrentDelegateState Updated (Initialized headId Closed)]
  _ <-
    runCompositeForAllDelegates $
      delegateStepOnExpectedHydraEvent
        (SpecificKind ReadyToFanoutKind)
        []
  _ <-
    runCompositeForAllDelegates $
      delegateStepOnExpectedHydraEvent
        (SpecificKind HeadIsFinalizedKind)
        [CurrentDelegateState Updated (Initialized headId Finalized)]
  return ()

emulateCleanup ::
  HasCallStack => AuctionTerms -> DelegatesClusterEmulator ()
emulateCleanup terms = do
  [] <-
    runCompositeForDelegate Main $
      delegateEventStepEmulated $
        AuctionStageStarted terms CleanupStage
  return ()

processSnapshotUpdates :: StateT DelegateState CompositeRunner ()
processSnapshotUpdates = go 15
  where
    go timeout = do
      mConfirmed <-
        waitForHydraEvent' timeout (SpecificKind SnapshotConfirmedKind)
      case mConfirmed of
        Just event -> do
          _ <- delegateEventStepEmulated $ HydraEvent event
          go 15
        Nothing -> return ()

placeNewBidOnL2AndCheck ::
  HasCallStack =>
  HeadId ->
  AuctionTerms ->
  Actor ->
  Natural ->
  StateT DelegateState CompositeRunner ()
placeNewBidOnL2AndCheck _headId terms bidder amount = do
  delegate <- askActor
  processSnapshotUpdates
  let fakeClientId = fromEnum delegate
  liftIO $
    putStrLn $
      "Placing bid by bidder: "
        <> show bidder
        <> " on delegate "
        <> show delegate
        <> " for "
        <> show amount
  (_, bidderSigningKey) <- liftIO $ keysFor bidder
  sellerSignature <- liftIO $ sellerSignatureForActor terms bidder
  let bidDatum = createStandingBidDatum terms amount sellerSignature bidderSigningKey
  submitNewBidToDelegate fakeClientId terms bidDatum
  void $ checkStandingBidWasUpdated bidDatum
  where
    checkStandingBidWasUpdated bidDatum = do
      let
        expectedBidTerms = standingBid $ standingBidState bidDatum
      delegateStepOnExpectedHydraEvent'
        (SpecificKind SnapshotConfirmedKind)
        (checkResponsesForBidTerms expectedBidTerms)

checkResponsesForBidTerms ::
  Maybe BidTerms -> [DelegateResponse] -> Bool
checkResponsesForBidTerms expectedBidTerms responses =
  case responses of
    [ CurrentDelegateState
        Updated
        (Initialized _ (Open (MkOpenHeadUtxo {standingBidTerms}) _))
      ] ->
        standingBidTerms == expectedBidTerms
    _ -> False

submitNewBidToDelegate :: ClientId -> AuctionTerms -> StandingBidDatum -> StateT DelegateState CompositeRunner ()
submitNewBidToDelegate fakeClientId terms bidDatum = do
  responses <-
    delegateFrontendRequestStep
      (fakeClientId, NewBid {auctionTerms = terms, datum = bidDatum})
  liftIO $
    assertEqual
      "New bid"
      []
      responses
