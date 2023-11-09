-- FIXME: rename to Emulator
module EndToEnd.HydraUtils (
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
import Hydra.Prelude (toList)
import HydraAuctionUtils.Prelude

-- Haskell import

-- import Control.Concurrent.Async (mapConcurrently)
import Control.Tracer (nullTracer)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv, setEnv)
import System.Process (system)
import Text.Read (readMaybe)

-- Hydra imports

import CardanoNode (
  RunningNode (..),
 )
import Hydra.Cardano.Api (TxId)
import Hydra.ContestationPeriod (
  ContestationPeriod (UnsafeContestationPeriod),
 )
import Hydra.Network (Host (..))
import HydraNode (HydraClient (..), waitForNodesConnected, withHydraCluster)
import Test.Hydra.Prelude (withTempDir)

-- HydraAuction imports
import HydraAuction.Delegate.Interface (DelegateProtocol)
import HydraAuction.Platform.Interface (PlatformProtocol)
import HydraAuction.Platform.Storage (PlatformImplementation)
import HydraAuctionUtils.BundledData (lookupProtocolParamPath)
import HydraAuctionUtils.Composite.Runner (CompositeRunner)
import HydraAuctionUtils.Delegate.Interface (
  DelegateState,
  initialState,
 )
import HydraAuctionUtils.Fixture (
  Actor (..),
  ActorKind (..),
  actorsByKind,
  hydraKeysFor,
  keysFor,
 )
import HydraAuctionUtils.Hydra.Interface (
  HydraConnectionConfig (..),
  HydraProtocol,
 )
import HydraAuctionUtils.Hydra.Runner (
  HydraExecutionContext,
  executeHydraRunner,
  executeHydraRunnerFakingParams,
  prepareScriptRegistry,
  runL1RunnerInComposite,
 )
import HydraAuctionUtils.L1.Runner (
  ExecutionContext (..),
  L1Runner,
  dockerNode,
  executeL1Runner,
  executeL1RunnerWithNode,
  executeTestL1Runner,
  withActor,
 )
import HydraAuctionUtils.Tx.Common (transferAda)
import HydraAuctionUtils.WebSockets.Client (
  FakeProtocolClient,
  ProtocolClientFor,
  RealProtocolClient (MkRealProtocolClient),
  newFakeClient,
  withProtocolClient,
 )
import HydraAuctionUtils.WebSockets.Protocol (WithClientT, withClient)

-- Ways to spin up Hydra cluster

-- This function will set the HYDRA_CONFIG_DIR env var locally
-- This is required so the hydra nodes pick up on the correct protocol-parameters.json
-- file.
-- FIXME: remove all of this: HYDRA_CONFIG_DIR hack, Hydra internal util usage,
-- and overlall separation of "spin-up" functions with not-clear invariants
withManualHydraCluster :: HasCallStack => TxId -> ([RealProtocolClient HydraProtocol] -> L1Runner ()) -> L1Runner ()
withManualHydraCluster hydraScriptsTxId cont = do
  liftIO $ do
    hydraDir <- lookupProtocolParamPath
    setEnv "HYDRA_CONFIG_DIR" hydraDir
  ctx@(MkExecutionContext {node}) <- ask
  seedHydraNodes
  liftIO $
    withTempDir "end-to-end-test" $ \tmpDir -> do
      cardanoKeys <- mapM keysFor actors
      hydraSks <- mapM (fmap snd . hydraKeysFor) actors
      let firstNodeId = 1 -- Other will probably not work
      let contestationPeriod = UnsafeContestationPeriod 2
      withHydraCluster
        nullTracer
        tmpDir
        (nodeSocket node)
        firstNodeId
        cardanoKeys
        hydraSks
        hydraScriptsTxId
        contestationPeriod
        $ \nodes -> do
          waitForNodesConnected nullTracer $ toList nodes
          executeL1Runner ctx $ do
            cont $ map clientFromNode $ toList nodes
  where
    actors = (Map.!) actorsByKind HydraNodeActor
    initActor actor = withActor Faucet $ transferAda actor 100_000_000
    seedHydraNodes = mapM_ initActor actors
    clientFromNode node =
      MkRealProtocolClient
        (Host "127.0.0.1" (fromIntegral $ 4_000 + hydraNodeId node))
        $ connection node

withDockerComposeCluster ::
  ([RealProtocolClient HydraProtocol] -> L1Runner b) ->
  IO b
withDockerComposeCluster cont = do
  _ <- system "./scripts/spin-up-new-devnet.sh 0"
  withNClients 3 (action . reverse)
  where
    withHydraClientN n =
      withProtocolClient
        (Host "127.0.0.1" (fromIntegral $ 4000 + n))
        (MkHydraConnectionConfig {retrieveHistory = False})
    withNClients' clientsAcc n cont' =
      if length clientsAcc == n
        then cont' clientsAcc
        else withHydraClientN (length clientsAcc + 1) $
          \client -> withNClients' (client : clientsAcc) n cont'
    withNClients = withNClients' []
    action nodes = do
      executeL1RunnerWithNode dockerNode (cont nodes)

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
runEmulatorInTest action = do
  mEnvStr <- liftIO $ lookupEnv "USE_DOCKER_FOR_TESTS"
  let useDockerForTests =
        fromMaybe False $ readMaybe =<< mEnvStr
  if useDockerForTests
    then
      liftIO $
        withDockerComposeCluster $
          flip runEmulator action
    else executeTestL1Runner $ do
      (!hydraScriptsTxId, _) <- prepareScriptRegistry Nothing
      withManualHydraCluster hydraScriptsTxId $
        flip runEmulator action

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
  -- context <- ask
  -- let runActionInIO delegate = do
  --       flip runReaderT context $
  --         unDelegatesClusterEmulator $
  --           runCompositeForDelegate delegate action
  -- liftIO $ mapM runActionInIO allDelegates
  mapM (flip runCompositeForDelegate action) allDelegates
