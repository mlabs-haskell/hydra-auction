{-# LANGUAGE StrictData #-}

module HydraAuction.Delegate.Runner (
  DelegateRunnerT (..),
  execDelegateRunnerT,
) where

-- Predule imports
import Prelude

-- Haskell imports

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT (..), evalStateT)
import Control.Monad.Trans (MonadTrans (lift))

-- HydraAuction imports
import HydraAuction.Delegate.Interface (
  DelegateState (..),
  initialState,
 )

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
