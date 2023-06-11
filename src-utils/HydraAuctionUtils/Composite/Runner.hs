module HydraAuctionUtils.Composite.Runner (
  CompositeRunner,
  CompositeExecutionContext (..),
  executeCompositeRunner,
  runHydraInComposite,
  runL1RunnerInComposite,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- HydraAuction imports

import HydraAuctionUtils.Hydra.Monad (MonadHydra (..))
import HydraAuctionUtils.Hydra.Runner (
  HydraExecutionContext,
  HydraRunner,
  executeHydraRunner,
 )
import HydraAuctionUtils.L1.Runner (ExecutionContext, L1Runner, executeL1Runner)
import HydraAuctionUtils.Monads.Actors (MonadHasActor (..))

-- | Contexts should use same actor internally
data CompositeExecutionContext = MkCompositeExecutionContext
  { hydraContext :: HydraExecutionContext
  , l1Context :: ExecutionContext
  }

newtype CompositeRunner a = MkCompositeRunner
  {unCompositeRunner :: ReaderT CompositeExecutionContext IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader CompositeExecutionContext
    , MonadBase IO
    , MonadBaseControl IO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

executeCompositeRunner ::
  forall x.
  CompositeExecutionContext ->
  CompositeRunner x ->
  IO x
executeCompositeRunner context action =
  runReaderT (unCompositeRunner action) context

runHydraInComposite :: forall a. HydraRunner a -> CompositeRunner a
runHydraInComposite action = do
  MkCompositeExecutionContext {hydraContext} <- ask
  liftIO $ executeHydraRunner hydraContext action

runL1RunnerInComposite :: forall a. L1Runner a -> CompositeRunner a
runL1RunnerInComposite action = do
  MkCompositeExecutionContext {l1Context} <- ask
  liftIO $ executeL1Runner l1Context action

instance MonadHydra CompositeRunner where
  sendCommand = runHydraInComposite . sendCommand
  waitForHydraEvent' timeout =
    runHydraInComposite . waitForHydraEvent' timeout

instance MonadHasActor CompositeRunner where
  askActor = runL1RunnerInComposite askActor
