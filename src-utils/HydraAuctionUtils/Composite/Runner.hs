module HydraAuctionUtils.Composite.Runner (
  CompositeRunner,
  CompositeExecutionContext (..),
  executeCompositeRunner,
  runHydraInComposite,
  runL1RunnerInComposite,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), ask)
import Control.Monad.Trans (MonadIO (liftIO))

-- HydraAuction imports
import HydraAuctionUtils.Hydra.Monad (MonadHydra (..))
import HydraAuctionUtils.Hydra.Runner (
  HydraExecutionContext,
  HydraRunner,
  executeHydraRunner,
 )
import HydraAuctionUtils.L1.Runner (ExecutionContext, Runner, executeRunner)
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

runL1RunnerInComposite :: forall a. Runner a -> CompositeRunner a
runL1RunnerInComposite action = do
  MkCompositeExecutionContext {l1Context} <- ask
  liftIO $ executeRunner l1Context action

instance MonadHydra CompositeRunner where
  sendCommand = runHydraInComposite . sendCommand
  waitForHydraEvent' timeout =
    runHydraInComposite . waitForHydraEvent' timeout

instance MonadHasActor CompositeRunner where
  askActor = runL1RunnerInComposite askActor
