{-# LANGUAGE AllowAmbiguousTypes #-}

module HydraAuctionUtils.Delegate.Logic (
  SingleClientScope (..),
  delegateFrontendRequestStep,
  delegateEventStep,
  abort,
  DelegateLogic (..),
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports

import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (
  CtxUTxO,
  Tx,
  TxIn,
  TxOut,
  getTxBody,
  getTxId,
  pattern ShelleyAddressInEra,
  pattern TxOut,
 )
import Hydra.Chain (HeadId, PostChainTx (..), PostTxError (..))
import Hydra.Snapshot (Snapshot (..))

-- HydraAuctionUtils imports

import HydraAuctionUtils.Delegate.Interface
import HydraAuctionUtils.Fixture (partyFor)
import HydraAuctionUtils.Hydra.Monad (MonadHydra (..))
import HydraAuctionUtils.Hydra.Runner (HydraRunner)
import HydraAuctionUtils.Monads.Actors (
  MonadHasActor (askActor),
  addressAndKeys,
 )
import HydraAuctionUtils.Tx.Common (
  createMinAdaUtxo,
  queryOrCreateSingleMinAdaUtxo,
 )
import HydraAuctionUtils.Tx.Utxo (
  filterAdaOnlyUtxo,
  filterNotAdaOnlyUtxo,
 )
import HydraAuctionUtils.Types (Layer (..))
import HydraAuctionUtils.WebSockets.ClientId (
  ClientId,
  ClientResponseScope (..),
 )
import HydraAuctionUtils.WebSockets.Protocol (
  ProtocolClientFor,
  WithClientT,
 )

data SingleClientScope = SameClient | BroadcastEveryone

class DelegateLogicTypes protocol => DelegateLogic protocol where
  type DelegatePlatformProtocol protocol

  performCommitAction ::
    HeadId ->
    CommitAction protocol ->
    HydraRunner [(SingleClientScope, DelegateResponse protocol)]
  performTxAction ::
    HeadId ->
    TxAction protocol ->
    OpenState protocol ->
    HydraRunner [(SingleClientScope, DelegateResponse protocol)]

  reactToCustomEvent ::
    DelegateState protocol ->
    CustomEvent protocol ->
    HydraRunner (Either AbortReason ())
  reactToTxInvalid ::
    Tx -> UTxO.UTxO -> OpenState protocol -> HydraRunner (Either AbortReason ())

  delegateEventHook ::
    forall client.
    (ProtocolClientFor (DelegatePlatformProtocol protocol) client) =>
    DelegateEvent protocol ->
    WithClientT
      client
      (StateT (DelegateState protocol) HydraRunner)
      ()

  openStateUpdatedHook :: OpenState protocol -> HydraRunner ()

  isCorrectCommit :: TxOut CtxUTxO -> Bool
  parseOpenStateFromUtxo ::
    (TxIn, TxOut CtxUTxO) -> (TxIn, TxOut CtxUTxO) -> Maybe (OpenState protocol)

-- Generic implementations

delegateFrontendRequestStep ::
  forall delegateProtocol.
  (DelegateLogic delegateProtocol) =>
  (ClientId, FrontendRequest delegateProtocol) ->
  StateT
    (DelegateState delegateProtocol)
    HydraRunner
    [(ClientResponseScope, DelegateResponse delegateProtocol)]
delegateFrontendRequestStep (clientId, request) = catchAllErrors $
  case request of
    QueryCurrentDelegateState -> do
      state <- get
      return [(PerClient clientId, CurrentDelegateState WasQueried state)]
    SubmitCommit action -> do
      state <- get
      case state of
        Initialized
          headId
          (AwaitingCommits {primaryCommitWasSubmitted = False}) ->
            lift $ translateScopes <$> performCommitAction headId action
        _ ->
          return
            [ (PerClient clientId, RequestIgnored $ WrongDelegateState state)
            ]
    SubmitTx action -> do
      state <- get
      case state of
        Initialized headId (Open openState) ->
          lift $ translateScopes <$> performTxAction headId action openState
        _ ->
          return
            [ (PerClient clientId, RequestIgnored $ WrongDelegateState state)
            ]
  where
    translateScopes = map translateScope
    translateScope (scope, response) = case scope of
      SameClient -> (PerClient clientId, response)
      BroadcastEveryone -> (Broadcast, response)
    catchAllErrors action = do
      result <- trySome action
      case result of
        Right x -> pure x
        Left someError -> pure [(PerClient clientId, ServerError $ show someError)]

delegateEventStep ::
  forall delegateProtocol client.
  ( DelegateLogic delegateProtocol
  , ProtocolClientFor (DelegatePlatformProtocol delegateProtocol) client
  ) =>
  DelegateEvent delegateProtocol ->
  WithClientT
    client
    (StateT (DelegateState delegateProtocol) HydraRunner)
    [DelegateResponse delegateProtocol]
delegateEventStep event = catchAllErrors $ do
  _ <- delegateEventHook event
  lift $ delegateEventStepCommon event
  where
    catchAllErrors action = do
      result <- trySome action
      case result of
        Right x -> pure x
        Left someError -> pure [ServerError $ show someError]

delegateEventStepCommon ::
  forall delegateProtocol.
  (DelegateLogic delegateProtocol) =>
  DelegateEvent delegateProtocol ->
  StateT
    (DelegateState delegateProtocol)
    HydraRunner
    [DelegateResponse delegateProtocol]
delegateEventStepCommon event = case event of
  Start -> do
    -- Preparing for collateral
    sendCommand Init
    _ <- runL1RunnerInComposite createMinAdaUtxo
    return []
  CustomEvent customEvent -> do
    state <- get
    result <- lift $ reactToCustomEvent state customEvent
    case result of
      Left abortReason -> abort abortReason
      Right () -> return [CustomEventHappened customEvent]
  HydraEvent Committed {utxo, party} -> do
    state <- get
    delegateParty <- getDelegateParty
    case state of
      Initialized _ (AwaitingCommits {primaryCommitWasSubmitted}) ->
        case UTxO.pairs $ filterNotAdaOnlyUtxo utxo of
          [(_, txOut)] | isCorrectCommit @delegateProtocol txOut ->
            case (party == delegateParty, primaryCommitWasSubmitted) of
              -- Cannot commit standing bid twice
              (_, True) -> abort $ ImpossibleHappened IncorrectCommit
              (True, False) ->
                updateStateAndResponse $
                  AwaitingCommits {primaryCommitWasSubmitted = True}
              (False, False) -> do
                result <- commitCollateralAda
                case result of
                  Right () ->
                    updateStateAndResponse $
                      AwaitingCommits {primaryCommitWasSubmitted = True}
                  Left reason -> abort reason
          [] ->
            -- ADA-only commit case (collateral)
            if primaryCommitWasSubmitted
              then return []
              else -- Standing bid commit should come first
                abort $ ImpossibleHappened IncorrectCommit
          _ -> abort $ ImpossibleHappened IncorrectCommit
      _ -> abort $ ImpossibleHappened IncorrectHydraEvent
  HydraEvent (SnapshotConfirmed {snapshot}) -> case snapshot of
    Snapshot {utxo} -> updateStateWithStandingBidOrAbort utxo
  HydraEvent TxValid {transaction} -> do
    return [TxEvent L2 Valid (getTxId $ getTxBody transaction)]
  HydraEvent TxInvalid {transaction, utxo} -> do
    state <- get
    case state of
      Initialized _ (Open openState) -> do
        reaction <- lift $ reactToTxInvalid transaction utxo openState
        case reaction of
          Left abortReason -> abort abortReason
          Right () ->
            return [TxEvent L2 Invalid (getTxId $ getTxBody transaction)]
      _ -> abort $ ImpossibleHappened IncorrectHydraEvent
  HydraEvent ReadyToFanout {} -> do
    sendCommand Fanout
    return []
  HydraEvent (HeadIsInitializing {headId}) -> do
    let newState = AwaitingCommits {primaryCommitWasSubmitted = False}
    put $ Initialized headId newState
    -- Yes, this is code duplication
    updateStateAndResponse newState
  HydraEvent (HeadIsOpen {utxo}) ->
    updateStateWithStandingBidOrAbort utxo
  HydraEvent HeadIsClosed {} -> do
    updateStateAndResponse Closed
  HydraEvent HeadIsFinalized {} -> do
    -- Restart Head
    -- FIXME: unable to hook that, and otherwise not cool
    [] <- delegateEventStepCommon Start
    updateStateAndResponse Finalized
  HydraEvent HeadIsAborted {} ->
    updateStateAndResponse Aborted
  HydraEvent hydraEvent -> case hydraEvent of
    InvalidInput {} -> abort RequiredHydraRequestFailed
    -- Before `CommandFailed` was returned in some cases,
    -- similar to `PostTxOnChainFailed`. Lets check if thats still the case :D
    -- FIXME: better parsing
    CommandFailed {} -> return []
    PostTxOnChainFailed {postChainTx, postTxError} ->
      case (postChainTx, postTxError) of
        (InitTx _, _)
          | postTxError `elem` [NoSeedInput, NotEnoughFuel] ->
              abort $ PrerequisiteMissing HydraInit
        (_, _) -> txFailedCase postChainTx
    _ -> return [] -- Some unknown cases
    where
      txFailedCase txTag = case txTag of
        -- This is okay cuz these are concurrent requests,
        -- only one of which will be fulfilled
        InitTx {} -> return []
        FanoutTx {} -> return []
        CollectComTx {} -> return []
        -- Preventing infinite loop of Abortions
        AbortTx {} -> ignoreStop
        CloseTx {} -> ignoreStop
        _ -> abort RequiredHydraRequestFailed
      ignoreStop = do
        -- FIXME: use logs
        liftIO $ putStrLn "Abort/Close failed"
        return []
  where
    updateStateWithStandingBidOrAbort utxo = do
      (delegateAddress, _, _) <- runL1RunnerInComposite addressAndKeys
      case UTxO.pairs $ filterNotAdaOnlyUtxo utxo of
        [(txIn, txOut)] ->
          case collateralUtxosOf delegateAddress of
            [collateralUtxo] ->
              case parseOpenStateFromUtxo (txIn, txOut) collateralUtxo of
                Just newOpenState -> do
                  lift $ openStateUpdatedHook newOpenState
                  updateStateAndResponse $ Open newOpenState
                _ -> abort' "Wrong main UTxO"
            _ -> abort' "Wrong number (not one) of collaterals"
        _ -> abort' "Wrong number (not one) of main UTxO"
      where
        abort' message = abort $ ImpossibleHappened $ IncorrectUtxoOnL2 message
        collateralUtxosOf address =
          UTxO.pairs $ UTxO.filter belongTo $ filterAdaOnlyUtxo utxo
          where
            belongTo (TxOut (ShelleyAddressInEra address') _ _ _) =
              address == address'
            belongTo _ = False
    commitCollateralAda = do
      -- Should exist cuz prepared on Start
      forCollateralUtxo <-
        runL1RunnerInComposite queryOrCreateSingleMinAdaUtxo
      sendCommand (Commit $ UTxO.fromPairs [forCollateralUtxo])
      return $ Right ()
    getDelegateParty = do
      delegateActor <- askActor
      liftIO $ partyFor delegateActor

abort ::
  ( MonadIO m
  , MonadState (DelegateState protocol) m
  , MonadFail m
  , MonadHydra m
  , DelegateLogicTypes protocol
  ) =>
  AbortReason ->
  m [DelegateResponse protocol]
abort reason = do
  state <- get
  if wasStopped state
    then do
      putStrLn "Ignoring abort: state is already stopped"
      return []
    else do
      let command = if wasOpened state then Close else Abort
      sendCommand command
      updateStateAndResponse $ AbortRequested reason

updateStateAndResponse ::
  forall m protocol.
  DelegateLogicTypes protocol =>
  (MonadState (DelegateState protocol) m, MonadFail m) =>
  InitializedState protocol ->
  m [DelegateResponse protocol]
updateStateAndResponse newInitializedState = do
  -- FIXME: log incorrect previous state
  -- FIXME: handle not initialized state error
  Initialized headId _ <- get
  let newState = Initialized headId newInitializedState
  put newState
  return [CurrentDelegateState Updated newState]
