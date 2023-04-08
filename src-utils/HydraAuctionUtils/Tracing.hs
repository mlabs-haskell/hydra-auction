{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module HydraAuctionUtils.Tracing (
  -- * tracing types

  -- ** tracing transformer
  TracerT (..),
  runWithTracer,
  runWithTracer',
  askTracer,

  -- ** tracing type class
  MonadTracer (..),
) where

-- Prelude imports
import Hydra.Prelude (ReaderT (runReaderT))
import Prelude

-- Haskell imports
import Control.Monad.Reader (
  MonadIO,
  MonadReader (ask),
  MonadTrans (lift),
 )
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter)
import Control.Tracer (Tracer, natTracer, traceWith)

{- | a monad that one can trace in
   the typeclass takes as argument the
   type that serves as the trace and the monad
   that you can trace in
-}
class MonadTracer t m | m -> t where
  -- | trace a message in the monad
  trace :: t -> m ()

-- | a transformer that provides a contravariant tracer
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

-- | run a tracer by first applying a natural transformation to the tracer
runWithTracer :: forall a t m n. (forall b. n b -> m b) -> Tracer n t -> TracerT t m a -> m a
runWithTracer natTrans tracer = flip runReaderT (natTracer natTrans tracer) . runTracerT

-- | like 'runWithTracer' but using 'id' as the natural transformation
runWithTracer' :: forall a t m. Tracer m t -> TracerT t m a -> m a
runWithTracer' = runWithTracer id

{- | ask the Tracer transformer for its Tracer, tthis just wraps the MonadReader instance
   method
-}
askTracer :: forall t m. Monad m => TracerT t m (Tracer m t)
askTracer = ask @(Tracer m t)

instance MonadTrans (TracerT t) where
  lift = TracerT . lift

instance (Monad m) => MonadTracer t (TracerT t m) where
  trace t = ask >>= lift . flip traceWith t
