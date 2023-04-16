{-# LANGUAGE StrictData #-}

module HydraAuction.Delegate (
  delegateFrontendRequestStep,
  delegateEventStep,
  DelegateEvent (..),
  DelegateRunnerT (..),
  ClientResponseScope (..),
  ClientId,
  execDelegateRunnerT,
  clientIsInScope,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT (..), evalStateT, get, put)
import Control.Monad.Trans (MonadTrans (lift))

-- HydraAuction imports

import Hydra.Chain.Direct.Tx (headIdToCurrencySymbol)
import HydraAuction.Delegate.Interface (
  DelegateResponse (..),
  DelegateState (..),
  FrontendRequest (..),
  InitializedState (..),
  RequestIgnoredReason (..),
  ResponseReason (..),
  initialState,
 )
import HydraAuction.Hydra.Interface (HydraEvent (..))
import HydraAuction.OnChain.Common (validAuctionTerms)
import HydraAuction.Types (AuctionStage (..), AuctionTerms (..))

data DelegateEvent
  = Start
  | AuctionStageStarted AuctionStage
  | HydraEvent HydraEvent

newtype DelegateRunnerT m x = MkDelegateRunner
  { unDelegateRunner :: StateT DelegateState m x
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFail
    , MonadState DelegateState
    , MonadIO
    )

execDelegateRunnerT :: Monad m => DelegateRunnerT m x -> m x
execDelegateRunnerT (MkDelegateRunner action) =
  evalStateT action initialState

instance MonadTrans DelegateRunnerT where
  lift = MkDelegateRunner . lift

type ClientId = Int

data ClientResponseScope
  = Broadcast
  | PerClient ClientId

clientIsInScope :: ClientId -> ClientResponseScope -> Bool
clientIsInScope clientId scope = case scope of
  Broadcast -> True
  PerClient expectedClientId -> clientId == expectedClientId

delegateFrontendRequestStep ::
  forall m.
  (MonadFail m) =>
  (ClientId, FrontendRequest) ->
  DelegateRunnerT m [(ClientResponseScope, DelegateResponse)]
delegateFrontendRequestStep (clientId, request) = case request of
  QueryCurrentDelegateState -> do
    state <- get
    return [(PerClient clientId, CurrentDelegateState WasQueried state)]
  -- FIXME: validate standing bid utxo and move it to hydra
  CommitStandingBid {auctionTerms} -> do
    state <- get
    case state of
      Initialized headId NotYetOpen -> do
        -- FIXME: validate delegates are matching real Hydra?
        let termsHaveSameHeadId =
              hydraHeadId auctionTerms == headIdToCurrencySymbol headId
        if not $
          validAuctionTerms auctionTerms
            && termsHaveSameHeadId
          then return [(PerClient clientId, RequestIgnored IncorrectData)]
          else do
            put $ Initialized headId HasCommit
            -- FIXME: broadcast DelegateState
            -- FIXME: this wont work on all other delegates
            return [(Broadcast, AuctionSet auctionTerms)]
      _ ->
        return
          [ (PerClient clientId, RequestIgnored $ WrongDelegateState state)
          ]
  NewBid _ -> do
    state <- get
    case state of
      Initialized _ (Open {}) -> do
        -- FIXME: check standing bid would work
        -- FIXME: place new bid
        -- FIXME: return closing transaction
        return [(PerClient clientId, ClosingTxTemplate)]
      _ ->
        return
          [ (PerClient clientId, RequestIgnored $ WrongDelegateState state)
          ]

delegateEventStep ::
  forall m.
  MonadFail m =>
  DelegateEvent ->
  DelegateRunnerT m [DelegateResponse]
delegateEventStep event = case event of
  -- FIXME: initialize Hydra
  Start -> return []
  -- FIXME: close Hydra node
  AuctionStageStarted BiddingEndedStage -> return []
  -- FIXME: abort Hydra node
  AuctionStageStarted VoucherExpiredStage -> return []
  AuctionStageStarted _ -> return []
  -- FIXME: read standing bid and update state on both cases
  -- FIXME: commit empty transaction
  HydraEvent (Committed _) -> return []
  HydraEvent (SnapshotConfirmed _) -> return []
  -- FIXME: fanout Hydra node
  HydraEvent ReadyToFanout -> return []
  HydraEvent (HeadIsInitializing headId) -> do
    put $ Initialized headId NotYetOpen
    -- Yes, this is code duplication
    updateStateAndResponse NotYetOpen
  HydraEvent HeadIsClosed -> do
    updateStateAndResponse Closed
  HydraEvent HeadIsFinalized ->
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
