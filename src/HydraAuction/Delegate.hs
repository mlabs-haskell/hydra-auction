module HydraAuction.Delegate (
  delegateStep,
  DelegateEvent (..),
  DelegateInput (..),
  DelegateRunnerT (..),
) where

-- Prelude imports
import Prelude

-- Haskell imports

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState, StateT (..), get, put)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (..))

-- HydraAuction imports

import HydraAuction.Delegate.Interface (
  DelegateError (..),
  DelegateResponse (..),
  FrontendKind (..),
  FrontendRequest (..),
  KnownFrontendRequest (..),
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
data DelegateState = NoClient | HasClient FrontendKind AuctionTerms

newtype DelegateRunnerT m x
  = MkDelegateRunner (StateT DelegateState (ExceptT DelegateError m) x)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState DelegateState
    , MonadError DelegateError
    )

instance MonadTrans DelegateRunnerT where
  lift = MkDelegateRunner . lift . lift

delegateStep ::
  forall m. Monad m => DelegateInput -> DelegateRunnerT m DelegateResponse
delegateStep input = case input of
  DelegateEvent Start -> return Okay -- FIXME: initialize Hydra
  -- FIXME: close Hydra node
  DelegateEvent (AuctionStageStarted BiddingEndedStage) -> return Okay
  -- FIXME: abort Hydra node
  DelegateEvent (AuctionStageStarted VoucherExpiredStage) -> return Okay
  DelegateEvent (AuctionStageStarted _) -> return Okay
  -- FIXME: commit empty transaction
  DelegateEvent (HydraEvent NodeCommitted) -> return Okay
  -- FIXME: fanout Hydra node
  DelegateEvent (HydraEvent ContestationTimeEnded) -> return Okay
  FrontendRequest (FrontendConnect terms clientKind) -> do
    -- FIXME: validate
    -- FIXME: validate AuctionTerms are valid
    result <- case clientKind of
      Seller -> return Okay -- FIXME: validate this is auction seller
      Bidder -> return Okay -- FIXME: check
    put $ HasClient clientKind terms
    return result

  -- FIXME: validate stanging bid utxo and move it to hydra
  FrontendRequest (KnownFrontendRequest request) -> do
    state <- get
    case state of
      NoClient -> throwError NoClientYet
      HasClient clientKind _ ->
        case request of
          CommitStandingBid _ -> do
            case clientKind of
              Bidder -> throwError WrongClientKind
              Seller ->
                -- FIXME: check bid UTxO and commit it
                return Okay
          NewBid _amount ->
            case clientKind of
              Seller -> throwError WrongClientKind
              Bidder ->
                -- FIXME: place new bid
                -- FIXME: return closing transaction
                return ClosingPreparedTransaction
