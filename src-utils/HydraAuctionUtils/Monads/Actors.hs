module HydraAuctionUtils.Monads.Actors (
  MonadHasActor (..),
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

class Monad m => MonadHasActor m where
  askActor :: m Actor

instance (MonadHasActor m, MonadTrans t, Monad (t m)) => MonadHasActor (t m) where
  askActor = lift askActor

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
