{-# LANGUAGE StrictData #-}

-- | Pure logic and Hydra communication used for Delegate server
module HydraAuction.Delegate (
  delegateFrontendRequestStep,
  delegateEventStep,
  DelegateEvent (..),
  ClientResponseScope (..),
  ClientId,
  clientIsInScope,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad.State (MonadState, StateT, get, put)
import Control.Monad.Trans (MonadIO (..), MonadTrans (lift))

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- HydraAuction imports

import Hydra.Chain.Direct.Tx (headIdToCurrencySymbol)
import HydraAuction.Delegate.CompositeRunner (
  CompositeRunner,
  runHydraInComposite,
  runL1RunnerInComposite,
 )
import HydraAuction.Delegate.Interface (
  AbortReason (..),
  DelegateResponse (..),
  DelegateState (..),
  FrontendRequest (..),
  ImposibleEvent (..),
  IncorrectRequestDataReason (..),
  InitializedState (..),
  MissingPrerequisite (..),
  RequestIgnoredReason (..),
  ResponseReason (..),
 )
import HydraAuction.Hydra.Interface (HydraCommand (..), HydraEvent (..))
import HydraAuction.Hydra.Monad (MonadHydra (..))
import HydraAuction.OnChain.Common (validAuctionTerms)
import HydraAuction.OnChain.StandingBid (validNewBidTerms)
import HydraAuction.Tx.Common (actorTipUtxo)
import HydraAuction.Tx.StandingBid (
  createNewBidTx,
  decodeInlineDatum,
  moveToHydra,
 )
import HydraAuction.Types (
  AuctionStage (..),
  AuctionTerms (..),
  StandingBidDatum (..),
  StandingBidState (..),
 )
import HydraAuctionUtils.Fixture (partyFor)
import HydraAuctionUtils.Monads (MonadHasActor (askActor), MonadQueryUtxo (..), MonadSubmitTx (..), UtxoQuery (..))
import HydraAuctionUtils.Tx.Utxo (
  extractSingleUtxo,
  filterAdaOnlyUtxo,
  filterNonFuelUtxo,
  filterNotAdaOnlyUtxo,
  txOutIsAdaOnly,
 )

data DelegateEvent
  = Start
  | AuctionStageStarted AuctionStage
  | HydraEvent HydraEvent
  deriving stock (Eq, Show)

type ClientId = Int

data ClientResponseScope
  = Broadcast
  | PerClient ClientId
  deriving stock (Eq, Show)

clientIsInScope :: ClientId -> ClientResponseScope -> Bool
clientIsInScope clientId scope = case scope of
  Broadcast -> True
  PerClient expectedClientId -> clientId == expectedClientId

delegateFrontendRequestStep ::
  (ClientId, FrontendRequest) ->
  StateT DelegateState CompositeRunner [(ClientResponseScope, DelegateResponse)]
delegateFrontendRequestStep (clientId, request) =
  case request of
    QueryCurrentDelegateState -> do
      state <- get
      return [(PerClient clientId, CurrentDelegateState WasQueried state)]
    CommitStandingBid {auctionTerms, utxoToCommit} -> do
      state <- get
      case state of
        Initialized headId NotYetOpen -> do
          -- FIXME: do not query
          standingBidUtxo <-
            lift $
              runL1RunnerInComposite $
                UTxO.pairs <$> queryUtxo (ByTxIns [utxoToCommit])
          case standingBidUtxo of
            [(txIn, txOut)] -> do
              validatingAuctionTerms headId auctionTerms $ do
                -- FIXME: Waiting for support by Hydra
                lift $
                  runL1RunnerInComposite $
                    moveToHydra headId auctionTerms (txIn, txOut)
                return [(Broadcast, AuctionSet auctionTerms)]
            [] ->
              return
                [
                  ( PerClient clientId
                  , RequestIgnored $ IncorrectRequestData TxIdDoesNotExist
                  )
                ]
            _ -> do
              responses <- abort $ ImpossibleHappened OnChainInvariantBreaks
              return $ map (Broadcast,) responses
        _ ->
          return
            [ (PerClient clientId, RequestIgnored $ WrongDelegateState state)
            ]
    NewBid {auctionTerms, datum} -> do
      state <- get
      case state of
        Initialized headId (Open {standingBidTerms}) -> do
          validatingAuctionTerms headId auctionTerms $ do
            let newBidTerms = standingBid $ standingBidState datum
            if validNewBidTerms auctionTerms standingBidTerms newBidTerms
              then do
                _ <- lift $ do
                  actor <- askActor
                  runHydraInComposite $ do
                    tx <- createNewBidTx auctionTerms actor datum
                    submitTx tx
                -- FIXME: return closing transaction
                return [(PerClient clientId, ClosingTxTemplate)]
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
  AuctionStageStarted BiddingEndedStage -> do
    state <- get
    case state of
      NotInitialized -> abort NobodyCommitedInTime
      Initialized {} -> do
        sendCommand Close
        return []
  AuctionStageStarted _ -> return []
  HydraEvent (CommandFailed commandName)
    -- This is okay cuz of concurrent requests,
    -- only one of which will be fulfilled
    | commandName `elem` ["Init", "Fanout"] -> return []
    | commandName == "Abort" -> do
        -- FIXME
        liftIO $ putStrLn "Abort failed"
        return []
    | otherwise -> abort RequiredHydraRequestFailed
  HydraEvent hydraEvent
    | notExpected -> abort RequiredHydraRequestFailed
    where
      notExpected = case hydraEvent of
        InvlidInput {} -> True
        PostTxOnChainFailed {} -> True
        _ -> False
  HydraEvent Committed {utxo, party} -> do
    state <- get
    case UTxO.pairs utxo of
      [(_, txOut)] -> do
        delegateActor <- askActor
        delegateParty <- liftIO $ partyFor delegateActor
        case (party == delegateParty, isStandingBidTx txOut, state) of
          (False, True, Initialized _ NotYetOpen) ->
            commitCollateralAda
          (False, True, Initialized _ HasCommit) ->
            abort $ ImpossibleHappened IncorrectStandingBidUtxoOnL2
          (False, False, Initialized _ NotYetOpen) ->
            abort $ ImpossibleHappened IncorrectStandingBidUtxoOnL2
          (False, False, Initialized _ HasCommit) ->
            if not $ txOutIsAdaOnly txOut
              then abort $ ImpossibleHappened IncorrectStandingBidUtxoOnL2
              else return []
          (True, _, Initialized _ NotYetOpen) ->
            updateStateAndResponse HasCommit
          (True, _, Initialized _ HasCommit) -> return []
          _ -> return [] -- Other stages shoud not be possible
      _ ->
        abort $ ImpossibleHappened IncorrectStandingBidUtxoOnL2
  HydraEvent (SnapshotConfirmed _txs utxo) ->
    updateStateWithStandingBidOrAbort utxo
  HydraEvent ReadyToFanout -> do
    sendCommand Fanout
    return []
  HydraEvent (HeadIsInitializing headId) -> do
    put $ Initialized headId NotYetOpen
    -- Yes, this is code duplication
    updateStateAndResponse NotYetOpen
  HydraEvent (HeadIsOpen utxo) ->
    updateStateWithStandingBidOrAbort utxo
  HydraEvent HeadIsClosed -> do
    updateStateAndResponse Closed
  HydraEvent HeadIsFinalized {} ->
    updateStateAndResponse Finalized
  HydraEvent HeadIsAborted {} ->
    updateStateAndResponse Aborted
  HydraEvent _ -> return []
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
    updateStateWithStandingBidOrAbort utxo =
      case UTxO.pairs $ filterNotAdaOnlyUtxo utxo of
        [(_, txOut)] -> case decodeStandingBidTerms txOut of
          Just standingBid -> updateStateAndResponse $ Open standingBid
          Nothing -> abort'
        _ -> abort'
      where
        abort' = abort $ ImpossibleHappened IncorrectStandingBidUtxoOnL2
    commitCollateralAda = do
      moneyToCommit <-
        lift $
          runL1RunnerInComposite $
            filterAdaOnlyUtxo . filterNonFuelUtxo <$> actorTipUtxo
      case extractSingleUtxo moneyToCommit of
        -- FIXME: check commited amount
        Just forCollateralUtxo -> do
          lift $
            runHydraInComposite $
              sendCommand (Commit forCollateralUtxo)
          updateStateAndResponse HasCommit
        Nothing ->
          abort $ PrerequisiteMissing AdaForCommit

abort ::
  ( MonadIO (t CompositeRunner)
  , MonadTrans t
  , MonadState DelegateState (t CompositeRunner)
  , MonadFail (t CompositeRunner)
  ) =>
  AbortReason ->
  t CompositeRunner [DelegateResponse]
abort reason = do
  lift $ runHydraInComposite $ sendCommand Abort
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
