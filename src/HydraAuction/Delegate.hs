{-# LANGUAGE StrictData #-}

module HydraAuction.Delegate (
  delegateFrontendRequestStep,
  delegateEventStep,
  DelegateEvent (..),
  DelegateRunnerT (..),
  execDelegateRunnerT,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT (..), evalStateT, get, put)
import Control.Monad.Trans (MonadTrans (lift))

-- HydraAuction imports
import HydraAuction.Delegate.Interface (
  DelegateResponse (..),
  FrontendRequest (..),
 )
import HydraAuction.Hydra.Interface (HydraEvent (..))
import HydraAuction.Types (AuctionStage (..), AuctionTerms (..))

data DelegateEvent
  = Start
  | AuctionStageStarted AuctionStage
  | HydraEvent HydraEvent

data DelegateState = NoAuction | HasAuction AuctionTerms

initialState :: DelegateState
initialState = NoAuction

newtype DelegateRunnerT m x = MkDelegateRunner
  { unDelegateRunner :: StateT DelegateState m x
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState DelegateState
    , MonadIO
    )

execDelegateRunnerT :: Monad m => DelegateRunnerT m x -> m x
execDelegateRunnerT (MkDelegateRunner action) = evalStateT action initialState

instance MonadTrans DelegateRunnerT where
  lift = MkDelegateRunner . lift

type ClientId = Int

data ClientResponseScope
  = Broadcast
  | PerClient ClientId

delegateFrontendRequestStep ::
  forall m.
  Monad m =>
  (ClientId, FrontendRequest) ->
  DelegateRunnerT m [(ClientResponseScope, DelegateResponse)]
delegateFrontendRequestStep (clientId, request) = case request of
  -- FIXME: validate standing bid utxo and move it to hydra
  CommitStandingBid {auctionTerms} -> do
    state <- get
    case state of
      NoAuction -> do
        put $ HasAuction auctionTerms
        -- FIXME: validate AuctionTerms are valid
        return [(Broadcast, AuctionSet auctionTerms)]
      HasAuction _ -> return [(PerClient clientId, AlreadyHasAuction)]
  NewBid _ -> do
    state <- get
    case state of
      NoAuction -> return [(PerClient clientId, HasNoAuction)]
      HasAuction _ -> do
        -- FIXME: place new bid
        -- FIXME: return closing transaction
        return [(PerClient clientId, ClosingTxTemplate)]

delegateEventStep ::
  forall m.
  Monad m =>
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
  -- FIXME: commit empty transaction
  HydraEvent (Committed _) -> return []
  -- FIXME: fanout Hydra node
  HydraEvent ReadyToFanout -> return []
  HydraEvent _ -> return []
