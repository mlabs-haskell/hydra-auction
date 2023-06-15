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
) where

-- Preludes import
import Hydra.Prelude (toList)
import HydraAuctionUtils.Prelude

-- Haskell import

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (finally)
import Control.Tracer (nullTracer)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Network.WebSockets (runClient)
import System.Environment (lookupEnv, setEnv)
import System.Process (system)
import Text.Read (readMaybe)

-- Hydra imports

import CardanoNode (
  RunningNode (..),
 )
import Hydra.Cluster.Faucet (Marked (..), seedFromFaucet_)
import Hydra.ContestationPeriod (
  ContestationPeriod (UnsafeContestationPeriod),
 )
import HydraNode (HydraClient (..), waitForNodesConnected, withHydraCluster)
import Test.Hydra.Prelude (withTempDir)

-- HydraAuction imports

import Hydra.Cardano.Api (TxId)
import HydraAuction.Delegate.Interface (DelegateState, initialState)
import HydraAuction.HydraHacks (prepareScriptRegistry)
import HydraAuctionUtils.BundledData (lookupProtocolParamPath)
import HydraAuctionUtils.Composite.Runner (CompositeRunner)
import HydraAuctionUtils.Fixture (
  Actor (..),
  ActorKind (..),
  actorsByKind,
  hydraKeysFor,
  keysFor,
 )
import HydraAuctionUtils.Hydra.Runner (
  executeHydraRunner,
  executeHydraRunnerFakingParams,
 )
import HydraAuctionUtils.L1.Runner (
  ExecutionContext (..),
  L1Runner,
  dockerNode,
  executeL1Runner,
  executeL1RunnerWithNode,
  executeL1RunnerWithNodeAs,
  executeTestL1Runner,
 )

-- This function will set the HYDRA_CONFIG_DIR env var locally
-- This is required so the hydra nodes pick up on the correct protocol-parameters.json
-- file.
withManualHydraCluster :: Int -> TxId -> ([HydraClient] -> L1Runner ()) -> L1Runner ()
withManualHydraCluster clusterIx hydraScriptsTxId cont = do
  liftIO $ do
    hydraDir <- lookupProtocolParamPath
    setEnv "HYDRA_CONFIG_DIR" hydraDir
  ctx@(MkExecutionContext {node}) <- ask
  liftIO $
    withTempDir "end-to-end-test" $ \tmpDir -> do
      let actors = (Map.!) actorsByKind HydraNodeActor
      cardanoKeys <- mapM keysFor actors
      hydraSks <- mapM (fmap snd . hydraKeysFor) actors
      let firstNodeId = clusterIx * 3
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
          seedHydraNodes node cardanoKeys
          executeL1Runner ctx $ cont $ toList nodes
  where
    seedHydraNodes node cardanoKeys = do
      let initActor mark key =
            seedFromFaucet_ node key 100_000_000 mark nullTracer
      mapM_ (initActor Fuel . fst) cardanoKeys
      mapM_ (initActor Normal . fst) cardanoKeys

withDockerComposeCluster ::
  ([HydraClient] -> L1Runner b) ->
  IO b
withDockerComposeCluster cont = do
  _ <- system "./scripts/spin-up-new-devnet.sh 0"
  withNClients 3 action
  where
    withHydraClientN n cont' = liftIO $
      runClient "127.0.0.1" (4000 + n) "/history=no" $
        \connection ->
          cont' $
            HydraClient
              { hydraNodeId = n
              , connection = connection
              , tracer = contramap show nullTracer
              }
    withNClients' clientsAcc n cont' =
      if length clientsAcc == n
        then cont' clientsAcc
        else withHydraClientN n $
          \client -> withNClients' (client : clientsAcc) n cont'
    withNClients = withNClients' []
    action' nodes = do
      waitForNodesConnected nullTracer nodes
      executeL1RunnerWithNode dockerNode (cont nodes)
    action nodes = finally (action' nodes) (system "./scripts/stop-demo.sh")

data EmulatorDelegate = Main | Second | Third
  deriving stock (Eq, Show, Enum, Bounded, Ord)

allDelegates :: [EmulatorDelegate]
allDelegates = [Main, Second, Third]

data EmulatorContext = MkEmulatorContext
  { l1Node :: RunningNode
  , clients :: EmulatorDelegateClients
  , delegateStatesRef :: MVar (Map EmulatorDelegate DelegateState)
  }

type EmulatorDelegateClients = Map EmulatorDelegate (HydraClient, Actor)
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

runEmulatorInTest :: DelegatesClusterEmulator () -> IO ()
runEmulatorInTest action = do
  mEnvStr <- liftIO $ lookupEnv "USE_DOCKER_FOR_TESTS"
  let useDockerForTests =
        fromMaybe False $ readMaybe =<< mEnvStr
  if useDockerForTests
    then
      liftIO $
        withDockerComposeCluster $
          flip runEmulator action . makeThreeClients actors
    else executeTestL1Runner $ do
      MkExecutionContext {node} <- ask
      -- FIXME: race condition on Faucet
      (!hydraScriptsTxId, !_) <- liftIO $ prepareScriptRegistry node
      withManualHydraCluster 0 hydraScriptsTxId $
        flip runEmulator action . makeThreeClients actors
  where
    actors = (Map.!) actorsByKind HydraNodeActor
    makeThreeClients actors nodes =
      Map.fromList $ zip allDelegates (zip nodes actors)

runEmulator :: EmulatorDelegateClients -> DelegatesClusterEmulator a -> L1Runner a
runEmulator clients action = do
  MkExecutionContext {node} <- ask
  intialStatesRef <-
    liftIO $
      newMVar $
        Map.fromList [(name, initialState) | name <- allDelegates]
  let context =
        MkEmulatorContext
          { l1Node = node
          , clients = clients
          , delegateStatesRef = intialStatesRef
          }
  liftIO $ flip runReaderT context $ unDelegatesClusterEmulator action

runL1InEmulator :: forall x. L1Runner x -> DelegatesClusterEmulator x
runL1InEmulator action = do
  node <- l1Node <$> ask
  liftIO $ executeL1RunnerWithNode node action

runCompositeForDelegate ::
  forall x.
  EmulatorDelegate ->
  StateT DelegateState CompositeRunner x ->
  DelegatesClusterEmulator x
runCompositeForDelegate name action = do
  MkEmulatorContext {l1Node, clients, delegateStatesRef} <- ask
  context <- liftIO $ createContext l1Node clients

  -- Get state
  states <- liftIO $ takeMVar delegateStatesRef
  let state = (Map.!) states name

  -- Run
  (result, newState) <-
    liftIO $
      executeHydraRunner
        context
        (runStateT action state)

  -- Update state
  liftIO $ putMVar delegateStatesRef $ Map.insert name newState states

  -- Return
  return result
  where
    createContext l1Node clients = do
      let (hydraClient, actor) = (Map.!) clients name
      executeL1RunnerWithNodeAs l1Node actor $
        executeHydraRunnerFakingParams hydraClient ask

runCompositeForAllDelegates ::
  forall x.
  StateT DelegateState CompositeRunner x ->
  DelegatesClusterEmulator [x]
runCompositeForAllDelegates action = do
  context <- ask
  let runActionInIO delegate = do
        flip runReaderT context $
          unDelegatesClusterEmulator $
            runCompositeForDelegate delegate action
  liftIO $ mapConcurrently runActionInIO allDelegates
