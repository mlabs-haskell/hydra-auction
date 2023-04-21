-- FIXME: rename to Emulator
module EndToEnd.HydraUtils (
  spinUpHeads,
  runningThreeNodesDockerComposeHydra,
  runCompositeForDelegate,
  EmulatorDelegate (..),
  DelegatesClusterEmulator,
  runCompositeForAllDelegates,
  runEmulator,
) where

-- Preludes import
import Hydra.Prelude (MonadIO (liftIO), contramap, toList)
import Prelude

-- Haskell import

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (
  MVar,
  newMVar,
  putMVar,
  takeMVar,
 )
import Control.Exception (finally)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (StateT (..))
import Control.Tracer (nullTracer)
import Data.Map (Map)
import Data.Map qualified as Map
import Network.WebSockets (runClient)
import System.Environment (setEnv)
import System.Process (system)

-- Hydra imports

import CardanoNode (
  RunningNode (..),
 )
import Hydra.Cluster.Faucet (Marked (..), seedFromFaucet_)
import Hydra.ContestationPeriod (
  ContestationPeriod (UnsafeContestationPeriod),
 )
import HydraNode (EndToEndLog (..), HydraClient (..), waitForNodesConnected, withHydraCluster)
import Test.Hydra.Prelude (withTempDir)

-- HydraAuction imports

import Hydra.Cardano.Api (TxId)
import HydraAuction.Delegate.CompositeRunner (
  CompositeExecutionContext (..),
  CompositeRunner,
  executeCompositeRunner,
 )
import HydraAuction.Delegate.Interface (DelegateState, initialState)
import HydraAuction.Hydra.Runner (executeHydraRunnerFakingParams)
import HydraAuction.Runner (ExecutionContext (MkExecutionContext, node, tracer), Runner, dockerNode, executeRunner, executeRunnerWithNodeAs)
import HydraAuctionUtils.BundledData (lookupProtocolParamPath)
import HydraAuctionUtils.Fixture (
  Actor (..),
  hydraKeysFor,
  hydraNodeActors,
  keysFor,
 )

-- This function will set the HYDRA_CONFIG_DIR env var locally
-- This is required so the hydra nodes pick up on the correct protocol-parameters.json
-- file.
spinUpHeads :: Int -> TxId -> (EmulatorDelegateClients -> Runner ()) -> Runner ()
spinUpHeads clusterIx hydraScriptsTxId cont = do
  liftIO $ do
    hydraDir <- lookupProtocolParamPath
    setEnv "HYDRA_CONFIG_DIR" hydraDir
  ctx@(MkExecutionContext {node, tracer}) <- ask
  let hydraTracer = nullTracer
  liftIO $
    withTempDir "end-to-end-test" $ \tmpDir -> do
      oscarKeys@(oscarCardanoVk, _) <- keysFor Oscar
      patriciaKeys@(patriciaCardanoVk, _) <- keysFor Patricia
      rupertKeys@(rupertCardanoVk, _) <- keysFor Rupert

      (_, oscarSk) <- hydraKeysFor Oscar
      (_, patriciaSk) <- hydraKeysFor Patricia
      (_, rupertSk) <- hydraKeysFor Rupert

      let cardanoKeys = [oscarKeys, patriciaKeys, rupertKeys]
          hydraKeys = [oscarSk, patriciaSk, rupertSk]

      let firstNodeId = clusterIx * 3
      let contestationPeriod = UnsafeContestationPeriod 2
      withHydraCluster
        hydraTracer
        tmpDir
        (nodeSocket node)
        firstNodeId
        cardanoKeys
        hydraKeys
        hydraScriptsTxId
        contestationPeriod
        $ \nodes -> do
          [n1, n2, n3] <- pure $ toList nodes
          waitForNodesConnected hydraTracer [n1, n2, n3]
          -- Funds to be used as fuel by Hydra protocol transactions + some other ada to use as collateral to commit into the head
          let faucetTracer = contramap FromFaucet hydraTracer
          seedFromFaucet_ node oscarCardanoVk 100_000_000 Fuel faucetTracer
          seedFromFaucet_ node patriciaCardanoVk 100_000_000 Fuel faucetTracer
          seedFromFaucet_ node rupertCardanoVk 100_000_000 Fuel faucetTracer
          seedFromFaucet_ node oscarCardanoVk 100_000_000 Normal faucetTracer
          seedFromFaucet_ node patriciaCardanoVk 100_000_000 Normal faucetTracer
          seedFromFaucet_ node rupertCardanoVk 100_000_000 Normal faucetTracer
          executeRunner ctx $ do
            let [actor1, actor2, actor3] = hydraNodeActors
                threeClients =
                  Map.fromList
                    [ (Main, (n1, actor1))
                    , (Second, (n2, actor2))
                    , (Third, (n3, actor3))
                    ]
             in cont threeClients

runningThreeNodesDockerComposeHydra ::
  (EmulatorDelegateClients -> IO b) ->
  IO b
runningThreeNodesDockerComposeHydra cont = do
  _ <- system "./scripts/spin-up-new-devnet.sh 0"

  -- FIXME: more relaible wait (not sure for what, guess sockets opening)
  threadDelay 2_000_000

  let [actor1, actor2, actor3] = hydraNodeActors

  runHydraClientN 1 $
    \n1 -> runHydraClientN 2 $
      \n2 -> runHydraClientN 3 $
        \n3 ->
          let threeClients =
                Map.fromList
                  [ (Main, (n1, actor1))
                  , (Second, (n2, actor2))
                  , (Third, (n3, actor3))
                  ]
           in finally
                (cont threeClients)
                (system "./scripts/stop-demo.sh")
  where
    runHydraClientN n cont' = liftIO $
      runClient "127.0.0.1" (4000 + n) "/history=no" $
        \connection ->
          cont' $
            HydraClient
              { hydraNodeId = n
              , connection = connection
              , tracer = contramap show nullTracer
              }

data EmulatorDelegate = Main | Second | Third
  deriving stock (Eq, Show, Enum, Bounded, Ord)

allDelegates :: [EmulatorDelegate]
allDelegates = [Main, Second, Third]

data EmulatorContext = MkEmulatorContext
  { clients :: EmulatorDelegateClients
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

runEmulatorUsingDockerCompose :: DelegatesClusterEmulator a -> IO a
runEmulatorUsingDockerCompose action = runningThreeNodesDockerComposeHydra $ executeRunnerWithNodeAs dockerNode Alice . flip runEmulator action

runEmulator :: EmulatorDelegateClients -> DelegatesClusterEmulator a -> Runner a
runEmulator clients action = do
  intialStatesRef <-
    liftIO $
      newMVar $
        Map.fromList [(name, initialState) | name <- allDelegates]
  let context =
        MkEmulatorContext
          { clients = clients
          , delegateStatesRef = intialStatesRef
          }
  liftIO $ flip runReaderT context $ unDelegatesClusterEmulator action

runCompositeForDelegate ::
  forall x.
  RunningNode ->
  EmulatorDelegate ->
  StateT DelegateState CompositeRunner x ->
  DelegatesClusterEmulator x
runCompositeForDelegate node name action = do
  MkEmulatorContext {clients, delegateStatesRef} <- ask
  context <- liftIO $ createContext clients

  -- Get state
  states <- liftIO $ takeMVar delegateStatesRef
  let state = (Map.!) states name

  -- Run
  (result, newState) <-
    liftIO $
      executeCompositeRunner
        context
        (runStateT action state)

  -- Update state
  liftIO $ putMVar delegateStatesRef $ Map.insert name newState states

  -- Return
  return result
  where
    createContext clients = do
      let (hydraClient, actor) = (Map.!) clients name
      executeRunnerWithNodeAs node actor $ do
        l1Context <- ask
        executeHydraRunnerFakingParams hydraClient $ do
          hydraContext <- ask
          return $ MkCompositeExecutionContext {hydraContext, l1Context}

runCompositeForAllDelegates ::
  forall x.
  RunningNode ->
  StateT DelegateState CompositeRunner x ->
  DelegatesClusterEmulator [x]
runCompositeForAllDelegates node action = do
  context <- ask
  let runActionInIO delegate = do
        flip runReaderT context $
          unDelegatesClusterEmulator $
            runCompositeForDelegate node delegate action
  liftIO $ mapConcurrently runActionInIO allDelegates
