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
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans (MonadTrans (lift))

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
  DelegateResponse (..),
  DelegateState (..),
  FrontendRequest (..),
  InitializedState (..),
  RequestIgnoredReason (..),
  ResponseReason (..),
 )
import HydraAuction.Hydra.Interface (HydraCommand (..), HydraEvent (..))
import HydraAuction.Hydra.Monad (MonadHydra (..))
import HydraAuction.OnChain.Common (validAuctionTerms)
import HydraAuction.Tx.Common (actorTipUtxo)
import HydraAuction.Tx.StandingBid (
  createNewBidTx,
  decodeInlineDatum,
  moveToHydra,
 )
import HydraAuction.Types (
  AuctionStage (..),
  AuctionTerms (..),
 )
import HydraAuctionUtils.Monads (MonadHasActor (askActor), MonadQueryUtxo (..), MonadSubmitTx (..), UtxoQuery (..))
import HydraAuctionUtils.Tx.Utxo (
  filterNonFuelUtxo,
  filterNotAdaOnlyUtxo,
 )

data DelegateEvent
  = Start
  | AuctionStageStarted AuctionStage
  | HydraEvent HydraEvent

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
delegateFrontendRequestStep (clientId, request) = case request of
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
          [standingBidSingleUtxo] -> do
            -- FIXME: validate delegates are matching real Hydra?
            let termsHaveSameHeadId =
                  hydraHeadId auctionTerms == headIdToCurrencySymbol headId
            if not $
              validAuctionTerms auctionTerms
                && termsHaveSameHeadId
              then return [(PerClient clientId, RequestIgnored IncorrectData)]
              else do
                -- FIXME: Waiting for support by
                lift $
                  runL1RunnerInComposite $
                    moveToHydra headId auctionTerms standingBidSingleUtxo
                -- FIXME: this wont work on all other delegates
                return [(Broadcast, AuctionSet auctionTerms)]
          -- FIXME: better error reporting
          _ -> return [(PerClient clientId, RequestIgnored IncorrectData)]
      _ ->
        return
          [ (PerClient clientId, RequestIgnored $ WrongDelegateState state)
          ]
  NewBid {auctionTerms, datum} -> do
    state <- get
    case state of
      Initialized _ (Open {}) -> do
        -- FIXME: check standing bid would work
        _ <- lift $ do
          actor <- askActor
          runHydraInComposite $ do
            tx <- createNewBidTx auctionTerms actor datum
            submitTx tx
        -- FIXME: return closing transaction
        return [(PerClient clientId, ClosingTxTemplate)]
      _ ->
        return
          [ (PerClient clientId, RequestIgnored $ WrongDelegateState state)
          ]

-- FIXME: return to polymorphism back from CompositeRunner
delegateEventStep ::
  DelegateEvent ->
  StateT DelegateState CompositeRunner [DelegateResponse]
delegateEventStep event = case event of
  Start -> do
    sendCommand Init
    return []
  AuctionStageStarted BiddingEndedStage -> do
    sendCommand Close
    return []
  -- FIXME: abort Hydra node
  AuctionStageStarted VoucherExpiredStage -> return []
  AuctionStageStarted _ -> return []
  HydraEvent (Committed _utxo) -> do
    -- FIXME: prevent double standing bids
    -- FIXME: filter single Utxo and check amount
    forCollateralUtxo <-
      lift $
        runL1RunnerInComposite $
          filterNonFuelUtxo <$> actorTipUtxo
    sendCommand (Commit forCollateralUtxo)
    updateStateAndResponse HasCommit
  HydraEvent (SnapshotConfirmed _txs utxo) -> do
    case UTxO.pairs $ filterNotAdaOnlyUtxo utxo of
      [(_, txOut)] -> case decodeInlineDatum txOut of
        Right standingBid -> updateStateAndResponse $ Open standingBid
        Left _ -> return [] -- TODO: abort and log
      _ -> return [] -- TODO: abort and log
  HydraEvent ReadyToFanout -> do
    sendCommand Fanout
    return []
  HydraEvent (HeadIsInitializing headId) -> do
    put $ Initialized headId NotYetOpen
    -- Yes, this is code duplication
    updateStateAndResponse NotYetOpen
  HydraEvent HeadIsOpen {} -> do
    -- FIXME: acutally standing bid is not neccessarily Nothing
    updateStateAndResponse $ Open Nothing
  HydraEvent HeadIsClosed -> do
    updateStateAndResponse Closed
  HydraEvent HeadIsFinalized {} ->
    updateStateAndResponse Finalized
  HydraEvent _ -> return []
  where
    updateStateAndResponse newInitializedState = do
      -- FIXME: log incorrect previous state
      -- FIXME: use optics?
      Initialized headId _ <- get
      let newState = Initialized headId newInitializedState
      put newState
      return [CurrentDelegateState Updated newState]
