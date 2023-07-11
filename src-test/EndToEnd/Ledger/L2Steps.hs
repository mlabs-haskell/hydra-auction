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
  delegateEventStep,
  delegateFrontendRequestStep,
 )
import HydraAuction.Delegate.Interface (
  CommitAction (..),
  CustomEvent (..),
  DelegateProtocol,
  DelegateResponse (..),
  DelegateState (..),
  FrontendRequest (..),
  InitializedState (..),
  OpenHeadUtxo (..),
  OpenState (..),
  ResponseReason (..),
  TxAction (..),
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
  AwaitedHydraEvent,
  waitForHydraEvent,
  waitForHydraEvent',
 )
import HydraAuctionUtils.Monads.Actors (MonadHasActor (..))
import HydraAuctionUtils.Server.Client (AwaitedOutput (..))
import HydraAuctionUtils.Server.ClientId (ClientId, ClientResponseScope (..))
import HydraAuctionUtils.Types.Natural (Natural)

-- HydraAuction test imports
import EndToEnd.HydraUtils (
  DelegateAction,
  DelegatesClusterEmulator,
  EmulatorDelegate (..),
  runCompositeForAllDelegates,
  runCompositeForDelegate,
 )

delegateStepOnExpectedHydraEvent' ::
  forall client.
  HasCallStack =>
  AwaitedHydraEvent ->
  ([DelegateResponse DelegateProtocol] -> Bool) ->
  DelegateAction client HydraEvent
delegateStepOnExpectedHydraEvent' eventSpec checkResponses = do
  Just event <- lift $ waitForHydraEvent eventSpec
  delegate <- askActor
  responses <- delegateEventStep $ HydraEvent event
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
  forall client.
  AwaitedHydraEvent ->
  [DelegateResponse DelegateProtocol] ->
  DelegateAction client HydraEvent
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

emulateCustomEvent ::
  CustomEvent DelegateProtocol -> DelegatesClusterEmulator ()
emulateCustomEvent event = do
  result <-
    runCompositeForDelegate Main $ delegateEventStep $ CustomEvent event
  liftIO $
    assertEqual "CustomEvent reaction" [CustomEventHappened event] result

emulateDelegatesStart :: HasCallStack => DelegatesClusterEmulator HeadId
emulateDelegatesStart = do
  runCompositeForDelegate Main $ do
    [] <- delegateEventStep Start
    return ()

  headId : _ <- runCompositeForAllDelegates $ do
    Just event@(HeadIsInitializing {headId}) <- lift $ waitForHydraEvent Any
    responses <- delegateEventStep $ HydraEvent event
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
      lift $
        delegateFrontendRequestStep
          ( 1
          , SubmitCommit $
              MoveStandingBidToL2
                { commitAuctionTerms = terms
                , utxoToCommit = standingBidSingleUtxo
                }
          )
    liftIO $
      assertEqual
        "Commit Main"
        [(Broadcast, CustomEventHappened $ AuctionSet terms)]
        responses

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
        checkIsOpen

  return ()
  where
    checkIsOpen [CurrentDelegateState Updated (Initialized _ (Open _))] = True
    checkIsOpen _ = False

emulateClosing ::
  HasCallStack => HeadId -> AuctionTerms -> DelegatesClusterEmulator ()
emulateClosing headId terms = do
  emulateCustomEvent $ AuctionStageStarted terms BiddingEndedStage

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
emulateCleanup terms =
  emulateCustomEvent $ AuctionStageStarted terms CleanupStage

placeNewBidOnL2AndCheck ::
  HasCallStack =>
  HeadId ->
  AuctionTerms ->
  Actor ->
  Natural ->
  DelegateAction client ()
placeNewBidOnL2AndCheck _headId terms bidder amount = do
  delegate <- askActor
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
        (const True)

checkResponsesForBidTerms ::
  Maybe BidTerms -> [DelegateResponse DelegateProtocol] -> Bool
checkResponsesForBidTerms expectedBidTerms responses =
  case responses of
    [ CurrentDelegateState
        Updated
        ( Initialized
            _
            (Open (MkOpenState (MkOpenHeadUtxo {standingBidTerms}) _))
          )
      ] ->
        standingBidTerms == expectedBidTerms
    _ -> False

submitNewBidToDelegate ::
  ClientId -> AuctionTerms -> StandingBidDatum -> DelegateAction client ()
submitNewBidToDelegate fakeClientId terms bidDatum = do
  responses <-
    lift $
      delegateFrontendRequestStep
        (fakeClientId, SubmitTx $ NewBid {txAuctionTerms = terms, datum = bidDatum})
  liftIO $
    assertEqual
      "New bid"
      []
      responses
