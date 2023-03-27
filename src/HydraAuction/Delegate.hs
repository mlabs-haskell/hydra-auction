{-# LANGUAGE StrictData #-}

module HydraAuction.Delegate (
  delegateStep,
  DelegateEvent (..),
  DelegateInput (..),
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

-- FIXME: add client authentication (where is issue on this)

data DelegateEvent
  = Start
  | AuctionStageStarted AuctionStage
  | HydraEvent HydraEvent

data DelegateInput
  = DelegateEvent DelegateEvent
  | FrontendRequest FrontendRequest

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

delegateStep ::
  forall m.
  Monad m =>
  DelegateInput ->
  DelegateRunnerT m [DelegateResponse]
delegateStep input = case input of
  -- FIXME: initialize Hydra
  DelegateEvent Start -> return []
  -- FIXME: close Hydra node
  DelegateEvent (AuctionStageStarted BiddingEndedStage) -> return []
  -- FIXME: abort Hydra node
  DelegateEvent (AuctionStageStarted VoucherExpiredStage) -> return []
  DelegateEvent (AuctionStageStarted _) -> return []
  -- FIXME: commit empty transaction
  DelegateEvent (HydraEvent (Committed _tx)) -> return []
  -- FIXME: fanout Hydra node
  DelegateEvent (HydraEvent ReadyToFanout) -> return []
  DelegateEvent (HydraEvent _) -> return [] -- FIXME: validate standing bid utxo and move it to hydra
  FrontendRequest (CommitStandingBid {auctionTerms}) -> do
    state <- get
    case state of
      NoAuction -> do
        put $ HasAuction auctionTerms
        return [] -- FIXME: validate AuctionTerms are valid
      HasAuction _ -> return [AlreadyHasAuction]
  FrontendRequest (NewBid _) -> do
    state <- get
    case state of
      NoAuction -> return [HasNoAuction]
      HasAuction _ -> do
        -- FIXME: place new bid
        -- FIXME: return closing transaction
        return [ClosingTxTemplate]
