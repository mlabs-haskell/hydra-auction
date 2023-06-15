{-# LANGUAGE AllowAmbiguousTypes #-}

module HydraAuctionUtils.Server.Protocol (
  Protocol (..),
  ProtocolClient (..),
  ProtocolClientFor,
  ProtocolServerLogic (..),
  WithClientT,
  MonadHasClient (..),
  withClient,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Monad.State.Class (MonadState (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)

-- HydraAuction imports
import HydraAuctionUtils.Server.ClientId (ClientId, ClientResponseScope)

type ToFromJSON x = (ToJSON x, FromJSON x)
type Defaults x = (Eq x, Show x)

class
  ( ToFromJSON (Input protocol)
  , ToFromJSON (Output protocol)
  , Defaults (Input protocol)
  , Defaults (Output protocol)
  , Defaults (OutputKind protocol)
  ) =>
  Protocol protocol
  where
  type Input protocol = input | input -> protocol
  type Output protocol = output | output -> protocol
  type OutputKind protocol = outputKind | outputKind -> protocol

  -- | Required for RealProtocolClient
  type ConnectionConfig protocol

  getOutputKind :: Output protocol -> OutputKind protocol
  configToConnectionPath :: ConnectionConfig protocol -> Text

class Protocol (ClientFor handle) => ProtocolClient handle where
  type ClientFor handle
  sendInputH ::
    MonadIO m => handle -> Input (ClientFor handle) -> m ()
  receiveOutputH ::
    MonadIO m => handle -> m (Maybe (Output (ClientFor handle)))

type ProtocolClientFor protocol client =
  (ProtocolClient client, ClientFor client ~ protocol)

{- | This class gives state-driven implementation
for WebSocket server responding logic.
You may derive working WS server or FakeClient from this automatically.
While for case of Delegate server you will also implementation
for Hydra/Domain event reaction and other logic thread.
-}
class
  Protocol (ImplementationFor implementation) =>
  ProtocolServerLogic implementation
  where
  type ImplementationFor implementation
  type State implementation = state | state -> implementation
  initialState :: State implementation
  implementation ::
    Monad m =>
    (ClientId, Input (ImplementationFor implementation)) ->
    StateT
      (State implementation)
      m
      [(ClientResponseScope, Output (ImplementationFor implementation))]

-- Monad transformer

newtype WithClientT client m a = MkWithClientT (ReaderT client m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader client
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

deriving newtype instance (MonadTrans (WithClientT client))
deriving newtype instance (MonadTransControl (WithClientT client))

class (Monad m, ProtocolClient client) => MonadHasClient client m where
  askClient :: m client

-- FIXUP: seems to do not work
instance
  (Monad m, ProtocolClient client) =>
  MonadHasClient client (WithClientT client m)
  where
  askClient = ask @client

instance
  (MonadState state m, ProtocolClient client) =>
  MonadState state (WithClientT client m)
  where
  state = lift . state

withClient ::
  forall m x client.
  ProtocolClient client =>
  Monad m =>
  client ->
  WithClientT client m x ->
  m x
withClient client (MkWithClientT action) = runReaderT action client
