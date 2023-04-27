module HydraAuctionUtils.Monads.Actors (
  MonadHasActor (..),
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad.Trans (MonadTrans (lift))

-- HydraAuctionUtils imports
import HydraAuctionUtils.Fixture (Actor)

class Monad m => MonadHasActor m where
  askActor :: m Actor

instance (MonadHasActor m, MonadTrans t, Monad (t m)) => MonadHasActor (t m) where
  askActor = lift askActor
