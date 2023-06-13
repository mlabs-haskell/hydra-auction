module HydraAuctionUtils.Monads.Actors (
  MonadHasActor (..),
  WithActorT (..),
  withActor,
  addressAndKeys,
  actorTipUtxo,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO
import Hydra.Cardano.Api (
  Address,
  PaymentKey,
  ShelleyAddr,
  SigningKey,
  VerificationKey,
 )

-- HydraAuctionUtils imports

import HydraAuctionUtils.Fixture (Actor)
import HydraAuctionUtils.Monads (
  MonadNetworkId (..),
  MonadQueryUtxo (..),
  UtxoQuery (..),
  addressAndKeysForActor,
 )

newtype WithActorT m a = MkWithActorT (ReaderT Actor m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader Actor
    , MonadTrans
    , MonadTransControl
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

class Monad m => MonadHasActor m where
  askActor :: m Actor

instance {-# OVERLAPPABLE #-} (MonadHasActor m, MonadTrans t, Monad (t m)) => MonadHasActor (t m) where
  askActor = lift askActor

instance Monad m => MonadHasActor (WithActorT m) where
  askActor = ask @Actor

withActor :: forall m x. Monad m => Actor -> WithActorT m x -> m x
withActor actor (MkWithActorT action) = runReaderT action actor

addressAndKeys ::
  (MonadHasActor m, MonadNetworkId m, MonadIO m) =>
  m
    ( Address ShelleyAddr
    , VerificationKey PaymentKey
    , SigningKey PaymentKey
    )
addressAndKeys = do
  actor <- askActor
  addressAndKeysForActor actor

actorTipUtxo ::
  (MonadHasActor m, MonadQueryUtxo m, MonadNetworkId m, MonadIO m) => m UTxO.UTxO
actorTipUtxo = do
  (address, _, _) <- addressAndKeys
  queryUtxo (ByAddress address)
