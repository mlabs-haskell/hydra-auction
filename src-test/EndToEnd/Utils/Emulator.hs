module EndToEnd.Utils.Emulator (
  runCompositeForDelegate,
  EmulatorDelegate (..),
  EmulatorContext (..),
  DelegatesClusterEmulator,
  runCompositeForAllDelegates,
  runL1InEmulator,
  runEmulator,
  runEmulatorInTest,
  DelegateAction,
) where

-- Preludes import
import HydraAuctionUtils.Prelude

-- Haskell import
import Data.Map qualified as Map

-- HydraAuction imports
import HydraAuction.Delegate.Interface (DelegateProtocol)
import HydraAuction.Platform.Interface (PlatformProtocol)
import HydraAuction.Platform.Storage (PlatformImplementation)
import HydraAuctionUtils.Composite.Runner (CompositeRunner)
import HydraAuctionUtils.Delegate.Interface (
  DelegateState,
  initialState,
 )
import HydraAuctionUtils.Fixture (
  ActorKind (..),
  actorsByKind,
 )
import HydraAuctionUtils.Hydra.Interface (HydraProtocol)
import HydraAuctionUtils.Hydra.Runner (
  HydraExecutionContext,
  executeHydraRunner,
  executeHydraRunnerFakingParams,
  runL1RunnerInComposite,
 )
import HydraAuctionUtils.L1.Runner (
  L1Runner,
  withActor,
 )
import HydraAuctionUtils.WebSockets.Client (
  FakeProtocolClient,
  ProtocolClientFor,
  RealProtocolClient,
  newFakeClient,
 )
import HydraAuctionUtils.WebSockets.Protocol (WithClientT, withClient)

import EndToEnd.Utils.HydraCluster (withHydraClusterInTest)

-- Implementation of Emulator

data EmulatorDelegate = Main | Second | Third
  deriving stock (Eq, Show, Enum, Bounded, Ord)

allDelegates :: [EmulatorDelegate]
allDelegates = [Main, Second, Third]

data EmulatorContext = MkEmulatorContext
  { platformClient :: FakeProtocolClient PlatformImplementation
  , clients :: EmulatorDelegateClients
  , delegateStatesRef ::
      MVar (Map EmulatorDelegate (DelegateState DelegateProtocol))
  }

type EmulatorDelegateClients =
  Map EmulatorDelegate HydraExecutionContext

newtype DelegatesClusterEmulator a = DelegatesClusterEmulator
  { unDelegatesClusterEmulator :: ReaderT EmulatorContext IO a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFail
    , MonadIO
    , MonadReader EmulatorContext
    )

runEmulatorInTest :: HasCallStack => DelegatesClusterEmulator () -> IO ()
runEmulatorInTest = withHydraClusterInTest . flip runEmulator

runEmulator ::
  [RealProtocolClient HydraProtocol] -> DelegatesClusterEmulator a -> L1Runner a
runEmulator clients action = do
  intialStatesRef <-
    liftIO $
      newMVar $
        Map.fromList [(name, initialState) | name <- allDelegates]
  platformClient <- liftIO newFakeClient
  contexts <- mapM createHydraContext (zip actors clients)
  let context =
        MkEmulatorContext
          { platformClient = platformClient
          , clients = Map.fromList $ zip allDelegates contexts
          , delegateStatesRef = intialStatesRef
          }
  liftIO $ flip runReaderT context $ unDelegatesClusterEmulator action
  where
    actors = (Map.!) actorsByKind HydraNodeActor
    createHydraContext (actor, client) =
      withActor actor $ executeHydraRunnerFakingParams client ask

-- Using Emulator

-- FIXME: this is bad
type DelegateAction client x =
  ProtocolClientFor PlatformProtocol client =>
  WithClientT client (StateT (DelegateState DelegateProtocol) CompositeRunner) x

runL1InEmulator :: forall x. L1Runner x -> DelegatesClusterEmulator x
runL1InEmulator action =
  runCompositeForDelegate Main $ runL1RunnerInComposite $ lift action

runCompositeForDelegate ::
  forall x.
  EmulatorDelegate ->
  DelegateAction (FakeProtocolClient PlatformImplementation) x ->
  DelegatesClusterEmulator x
runCompositeForDelegate name action = do
  -- putStrLn $ "Doing action for delegate  " <> show name <> " ..."

  MkEmulatorContext {platformClient, clients, delegateStatesRef} <- ask

  -- Get state
  states <- liftIO $ takeMVar delegateStatesRef
  let state = (Map.!) states name
  let context = (Map.!) clients name

  -- Run
  (result, newState) <-
    liftIO $
      executeHydraRunner context (runStateT (withClient platformClient action) state)

  -- Update state
  liftIO $ putMVar delegateStatesRef $ Map.insert name newState states

  -- Return
  return result

runCompositeForAllDelegates ::
  forall x.
  HasCallStack =>
  DelegateAction (FakeProtocolClient PlatformImplementation) x ->
  DelegatesClusterEmulator [x]
runCompositeForAllDelegates action = do
  mapM (flip runCompositeForDelegate action) allDelegates
