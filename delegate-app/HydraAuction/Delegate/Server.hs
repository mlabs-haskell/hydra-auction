{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module HydraAuction.Delegate.Server (
  -- * Delegate server types
  DelegateServerConfig (..),
  DelegateServerLog (..),
  DelegateError (..),

  -- * tracing types

  -- ** tracing transformer
  TracerT (..),
  runWithTracer,
  runWithTracer',
  mapTracerT,
  askTracer,

  -- ** tracing type class
  MonadTracer (..),

  -- ** delegate tracing
  DelegateTracer,

  -- * wai extras
  ServerAppT,
) where

-- Prelude imports
import Hydra.Prelude (ReaderT (runReaderT), withReaderT)
import Prelude

-- Haskell imports
import Control.Tracer (Contravariant (contramap), Tracer, natTracer, traceWith)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty), line, viaShow)

-- Cardano imports
import Hydra.Network (IP, PortNumber)

-- Hydra auction imports

import Control.Monad.RWS
import HydraAuction.Delegate.Interface (DelegateResponse, FrontendRequest)
import Network.WebSockets (PendingConnection)

-- | The config for the delegate server
data DelegateServerConfig = DelegateServerConfig
  { dlgt'host :: IP
  -- ^ the host of the delegate server
  , dlgt'port :: PortNumber
  -- ^ the port number the delegate server receives input at
  , dlgt'tick :: Int
  -- ^ the amount of milliseconds, the thread should wait
  }

data DelegateServerLog
  = Started PortNumber
  | FrontendConnected
  | FrontendInput FrontendRequest
  | DelegateOutput DelegateResponse
  | DelegateError DelegateError
  deriving stock (Eq, Show, Generic)

instance Pretty DelegateServerLog where
  pretty = \case
    Started port -> "Started Server at Port" <> pretty (show port)
    FrontendConnected -> "Frontend connected to Server"
    DelegateOutput out -> "Delegate output" <> line <> viaShow out
    FrontendInput inp -> "Frontend input" <> line <> viaShow inp
    DelegateError err -> "Delegate error" <> line <> pretty err

-- FIXME: needs actual error
data DelegateError = FrontendNoParse
  deriving stock (Eq, Show, Generic)

instance Pretty DelegateError where
  pretty = \case
    FrontendNoParse -> "Could not parse the input provided by the frontend"

type DelegateTracer = TracerT DelegateServerLog

class MonadTracer t m | m -> t where
  trace :: t -> m ()

-- | a transformer that provides a tracer
newtype TracerT t m a = TracerT {runTracerT :: ReaderT (Tracer m t) m a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Tracer m t)
    , MonadIO
    )

-- these could be derived in a normal deriving clause but I feel better specifying
-- the context myself
deriving newtype instance MonadWriter w m => MonadWriter w (TracerT t m)
deriving newtype instance MonadState s m => MonadState s (TracerT t m)

mapTracerT :: (t1 -> t2) -> TracerT t1 m a -> TracerT t2 m a
mapTracerT f = TracerT . withReaderT (contramap f) . runTracerT

runWithTracer :: (forall b. n b -> m b) -> Tracer n t -> TracerT t m a -> m a
runWithTracer natTrans tracer = flip runReaderT (natTracer natTrans tracer) . runTracerT

runWithTracer' :: Tracer m t -> TracerT t m a -> m a
runWithTracer' = runWithTracer id

askTracer :: forall t m. Monad m => TracerT t m (Tracer m t)
askTracer = ask @(Tracer m t)

instance MonadTrans (TracerT t) where
  lift = TracerT . lift

instance (Monad m) => MonadTracer t (TracerT t m) where
  trace t = ask >>= lift . flip traceWith t

type ServerAppT m = PendingConnection -> m ()
