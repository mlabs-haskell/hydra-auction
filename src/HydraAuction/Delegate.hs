{-# LANGUAGE StrictData #-}

-- | Pure logic and Hydra communication used for Delegate server
module HydraAuction.Delegate (
  delegateFrontendRequestStep,
  delegateEventStep,
  DelegateEvent (..),
  abort,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad.State (MonadState, StateT, get, put)
import Control.Monad.Trans (MonadIO (..), MonadTrans (lift))

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports

import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (
  toPlutusKeyHash,
  verificationKeyHash,
  pattern ShelleyAddressInEra,
  pattern TxOut,
 )
import Hydra.Chain (PostChainTx (..), PostTxError (..))
import Hydra.Snapshot (Snapshot (..))

-- HydraAuction imports

import Hydra.Chain.Direct.Tx (headIdToCurrencySymbol)
import HydraAuction.Delegate.Interface (
  AbortReason (..),
  DelegateResponse (..),
  DelegateState (..),
  FrontendRequest (..),
  ImposibleEvent (..),
  IncorrectRequestDataReason (..),
  InitializedState (..),
  MissingPrerequisite (..),
  OpenHeadUtxo (..),
  RequestIgnoredReason (..),
  ResponseReason (..),
  wasOpened,
 )
import HydraAuction.OnChain.Common (validAuctionTerms)
import HydraAuction.OnChain.StandingBid (validNewBidTerms)
import HydraAuction.Tx.Common (createTwoMinAdaUtxo)
import HydraAuction.Tx.FeeEscrow (
  distributeFee,
 )
import HydraAuction.Tx.StandingBid (
  NewBidTxInfo (..),
  createNewBidTx,
  decodeInlineDatum,
  decodeNewBidTxOnL2,
  moveToHydra,
 )
import HydraAuction.Types (
  AuctionStage (..),
  AuctionTerms (..),
  StandingBidDatum (..),
  StandingBidState (..),
 )
import HydraAuctionUtils.Composite.Runner (
  CompositeRunner,
  runHydraInComposite,
  runL1RunnerInComposite,
 )
import HydraAuctionUtils.Fixture (partyFor)
import HydraAuctionUtils.Hydra.Interface (HydraEvent)
import HydraAuctionUtils.Hydra.Monad (MonadHydra (..))
import HydraAuctionUtils.Monads (
  MonadSubmitTx (..),
 )
import HydraAuctionUtils.Monads.Actors (
  MonadHasActor (askActor),
  addressAndKeys,
 )
import HydraAuctionUtils.Server.ClientId (
  ClientId,
  ClientResponseScope (..),
 )
import HydraAuctionUtils.Tx.Utxo (
  filterAdaOnlyUtxo,
  filterNotAdaOnlyUtxo,
 )

data DelegateEvent
  = Start
  | AuctionStageStarted AuctionTerms AuctionStage
  | HydraEvent HydraEvent
  deriving stock (Eq, Show)

delegateFrontendRequestStep ::
  (ClientId, FrontendRequest) ->
  StateT DelegateState CompositeRunner [(ClientResponseScope, DelegateResponse)]
delegateFrontendRequestStep (clientId, request) =
  case request of
    QueryCurrentDelegateState -> do
      state <- get
      return [(PerClient clientId, CurrentDelegateState WasQueried state)]
    CommitStandingBid {auctionTerms, utxoToCommit} -> do
      let (txIn, txOut) = utxoToCommit
      state <- get
      case state of
        Initialized headId (AwaitingCommits {stangingBidWasCommited = False}) ->
          validatingAuctionTerms headId auctionTerms $ do
            -- FIXME: Waiting for support by Hydra
            lift $
              runL1RunnerInComposite $
                moveToHydra headId auctionTerms (txIn, txOut)
            return [(Broadcast, AuctionSet auctionTerms)]
        _ ->
          return
            [ (PerClient clientId, RequestIgnored $ WrongDelegateState state)
            ]
    NewBid {auctionTerms, datum} -> do
      state <- get
      actor <- askActor
      case state of
        Initialized headId (Open utxoState _) -> do
          let (MkOpenHeadUtxo {standingBidTerms, standingBidUtxo, collateralUtxo}) =
                utxoState
          validatingAuctionTerms headId auctionTerms $ do
            let newBidTerms = standingBid $ standingBidState datum
                newVoucherCS = standingBidVoucherCS datum
            if validNewBidTerms auctionTerms newVoucherCS standingBidTerms newBidTerms
              then do
                lift $ runHydraInComposite $ do
                  tx <-
                    createNewBidTx
                      auctionTerms
                      actor
                      standingBidUtxo
                      collateralUtxo
                      datum
                  submitTx tx
                -- Caching AuctionTerms on this server
                put $
                  Initialized headId $
                    Open utxoState (Just auctionTerms)
                return []
              else
                return
                  [
                    ( PerClient clientId
                    , RequestIgnored $ IncorrectRequestData InvalidBidTerms
                    )
                  ]
        _ ->
          return
            [ (PerClient clientId, RequestIgnored $ WrongDelegateState state)
            ]
  where
    validatingAuctionTerms headId auctionTerms cont =
      -- FIXME: On M6: validate delegates are matching actual Head Peers
      let termsHaveSameHeadId =
            hydraHeadId auctionTerms == headIdToCurrencySymbol headId
       in if not $
            validAuctionTerms auctionTerms
              && termsHaveSameHeadId
            then
              return
                [
                  ( PerClient clientId
                  , RequestIgnored $
                      IncorrectRequestData
                        AuctionTermsAreInvalidOrNotMatchingHead
                  )
                ]
            else cont

-- FIXME: return to polymorphism back from CompositeRunner
delegateEventStep ::
  DelegateEvent ->
  StateT DelegateState CompositeRunner [DelegateResponse]
delegateEventStep event = case event of
  Start -> do
    sendCommand Init
    return []
  AuctionStageStarted _ BiddingEndedStage -> do
    state <- get
    case state of
      NotInitialized -> abort NobodyCommitedInTime
      Initialized {} -> do
        sendCommand Close
        return []
  AuctionStageStarted terms CleanupStage -> do
    -- FIXME: handle case of not used Escrow Hydra
    _ <- lift $ runL1RunnerInComposite $ distributeFee terms
    return []
  AuctionStageStarted {} -> return []
  HydraEvent Committed {utxo, party} -> do
    state <- get
    delegateParty <- getDelegateParty
    case state of
      Initialized _ (AwaitingCommits {stangingBidWasCommited}) ->
        case UTxO.pairs $ filterNotAdaOnlyUtxo utxo of
          [(_, txOut)] | isStandingBidTx txOut ->
            case (party == delegateParty, stangingBidWasCommited) of
              -- Cannot commit standing bid twice
              (_, True) -> abort $ ImpossibleHappened IncorrectCommit
              (True, False) ->
                updateStateAndResponse $
                  AwaitingCommits {stangingBidWasCommited = True}
              (False, False) -> do
                result <- commitCollateralAda
                case result of
                  Right () ->
                    updateStateAndResponse $
                      AwaitingCommits {stangingBidWasCommited = True}
                  Left reason -> abort reason
          [] ->
            -- ADA-only commit case (collateral)
            if stangingBidWasCommited
              then return []
              else -- Standing bid commit should come first
                abort $ ImpossibleHappened IncorrectCommit
          _ -> abort $ ImpossibleHappened IncorrectCommit
      _ -> abort $ ImpossibleHappened IncorrectHydraEvent
  HydraEvent (SnapshotConfirmed {snapshot}) -> case snapshot of
    Snapshot {utxo} -> updateStateWithStandingBidOrAbort utxo
  HydraEvent (TxInvalid {utxo, transaction}) -> do
    state <- get
    actor <- askActor
    (_, delegatePublicKey, _) <- lift $ runL1RunnerInComposite addressAndKeys
    case state of
      Initialized _ (Open utxoState mCachedTerms) -> do
        case (txResentRequired delegatePublicKey, mCachedTerms) of
          (Just newBidDatum, Just auctionTerms) -> do
            lift $ resentBid actor utxoState auctionTerms newBidDatum
            return []
          (Just _, Nothing) ->
            -- If tx was sent initialy by this server,
            -- auction terms should be already cached
            abort $ ImpossibleHappened IncorrectHydraEvent
          (Nothing, _) -> return []
      _ -> abort $ ImpossibleHappened IncorrectHydraEvent
    where
      txResentRequired delegatePublicKey =
        let
          delegatePKH =
            toPlutusKeyHash $
              verificationKeyHash delegatePublicKey
         in
          case decodeNewBidTxOnL2 transaction utxo of
            Just
              ( MkNewBidTxInfo
                  { submitterPKH
                  , isStandingBidInvalidated
                  , newBidDatum
                  }
                ) ->
                if submitterPKH == delegatePKH
                  && isStandingBidInvalidated
                  then Just newBidDatum
                  else Nothing
            Nothing -> Nothing
      resentBid actor utxoState auctionTerms newBidDatum = do
        let (MkOpenHeadUtxo {standingBidUtxo, collateralUtxo}) = utxoState
        runHydraInComposite $ do
          newTx <-
            createNewBidTx
              auctionTerms
              actor
              standingBidUtxo
              collateralUtxo
              newBidDatum
          submitTx newTx
  HydraEvent ReadyToFanout {} -> do
    sendCommand Fanout
    return []
  HydraEvent (HeadIsInitializing {headId}) -> do
    let newState = AwaitingCommits {stangingBidWasCommited = False}
    put $ Initialized headId newState
    -- Yes, this is code duplication
    updateStateAndResponse newState
  HydraEvent (HeadIsOpen {utxo}) ->
    updateStateWithStandingBidOrAbort utxo
  HydraEvent HeadIsClosed {} -> do
    updateStateAndResponse Closed
  HydraEvent HeadIsFinalized {} ->
    updateStateAndResponse Finalized
  HydraEvent HeadIsAborted {} ->
    updateStateAndResponse Aborted
  HydraEvent hydraEvent -> case hydraEvent of
    InvalidInput {} -> abort RequiredHydraRequestFailed
    -- Before `CommandFailed` was returned in some cases,
    -- similar to `PostTxOnChainFailed`. Lets check if thats still the case :D
    CommandFailed {} -> abort RequiredHydraRequestFailed
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
    decodeStandingBidTerms txOut = case decodeInlineDatum txOut of
      Right standingBidDatum ->
        let StandingBidDatum {standingBidState} = standingBidDatum
            StandingBidState {standingBid} = standingBidState
         in Just standingBid
      Left _ -> Nothing
    isStandingBidTx txOut = case decodeInlineDatum txOut of
      Right (_ :: StandingBidDatum) -> True
      Left _ -> False
    updateStateWithStandingBidOrAbort utxo = do
      state <- get
      let previousCache = case state of
            Initialized _ (Open _ x) -> x
            _ -> Nothing
      (delegateAddress, _, _) <- lift $ runL1RunnerInComposite addressAndKeys
      case UTxO.pairs $ filterNotAdaOnlyUtxo utxo of
        [(txIn, txOut)] ->
          case ( decodeStandingBidTerms txOut
               , collateralUtxosOf delegateAddress
               ) of
            (Just standingBidDatum, [collateralUtxo]) -> do
              let utxoState =
                    MkOpenHeadUtxo
                      { standingBidTerms = standingBidDatum
                      , standingBidUtxo = (txIn, txOut)
                      , collateralUtxo = collateralUtxo
                      }
              updateStateAndResponse $ Open utxoState previousCache
            _ -> abort'
        _ -> abort'
      where
        abort' = abort $ ImpossibleHappened IncorrectStandingBidUtxoOnL2
        collateralUtxosOf address =
          UTxO.pairs $ UTxO.filter belongTo $ filterAdaOnlyUtxo utxo
          where
            belongTo (TxOut (ShelleyAddressInEra address') _ _ _) =
              address == address'
            belongTo _ = False
    commitCollateralAda = do
      (forCollateralUtxo, _) <-
        lift $ runL1RunnerInComposite createTwoMinAdaUtxo
      sendCommand (Commit $ UTxO.fromPairs [forCollateralUtxo])
      return $ Right ()
    getDelegateParty = do
      delegateActor <- askActor
      liftIO $ partyFor delegateActor

abort ::
  ( MonadIO (t CompositeRunner)
  , MonadTrans t
  , MonadState DelegateState (t CompositeRunner)
  , MonadFail (t CompositeRunner)
  ) =>
  AbortReason ->
  t CompositeRunner [DelegateResponse]
abort reason = do
  state <- get
  let command = if wasOpened state then Close else Abort
  lift $ runHydraInComposite $ sendCommand command
  updateStateAndResponse $ AbortRequested reason

updateStateAndResponse ::
  (MonadState DelegateState m, MonadFail m) =>
  InitializedState ->
  m [DelegateResponse]
updateStateAndResponse newInitializedState = do
  -- FIXME: log incorrect previous state
  -- FIXME: handle not initialized state error
  Initialized headId _ <- get
  let newState = Initialized headId newInitializedState
  put newState
  return [CurrentDelegateState Updated newState]
