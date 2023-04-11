module EndToEnd.HydraUtils (
  runningThreeNodesDockerComposeHydra,
  runEmulatorUsingDockerCompose,
  runCompositeForDelegate,
  EmulatorDelegate (..),
  runCompositeForAllDelegates,
) where

-- Preludes import
import Hydra.Prelude (MonadIO (liftIO), contramap)
import Prelude

-- Haskell import

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Tracer (stdoutTracer)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.WebSockets (runClient)
import System.Process (system)

-- Hydra imports

import HydraNode (HydraClient (..))

-- HydraAuction imports

import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (StateT (..))
import Data.Map (Map)
import Data.Map qualified as Map
import HydraAuction.Delegate.CompositeRunner (
  CompositeExecutionContext (..),
  CompositeRunner,
  executeCompositeRunner,
 )
import HydraAuction.Delegate.Interface (DelegateState, initialState)
import HydraAuction.Hydra.Runner (executeHydraRunnerFakingParams)
import HydraAuction.Runner (executeRunnerWithLocalNode, withActor)
import HydraAuctionUtils.Fixture (
  Actor (..),
  hydraNodeActors,
 )

runningThreeNodesDockerComposeHydra ::
  (EmulatorDelegateClients -> IO b) ->
  IO b
runningThreeNodesDockerComposeHydra cont = do
  _ <- system "./scripts/spin-up-new-devnet.sh"

  -- FIXME: more relaible wait (not sure for what, guess sockets opening)
  threadDelay 2_000_000

  [actor1, actor2, actor3] <- return hydraNodeActors

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
                (system "docker-compose down")
  where
    runHydraClientN n cont' = liftIO $
      runClient "127.0.0.1" (4000 + n) "/history=no" $
        \connection ->
          cont' $
            HydraClient
              { hydraNodeId = n
              , connection = connection
              , tracer = hydraTracerN n
              }
    hydraTracerN n =
      contramap
        (\x -> "Hydra client for node " <> show n <> " :" <> show x)
        stdoutTracer

data EmulatorDelegate = Main | Second | Third
  deriving stock (Eq, Show, Enum, Bounded, Ord)

allDelegates :: [EmulatorDelegate]
allDelegates = [Main, Second, Third]

data EmulatorContext = MkEmulatorContext
  { clients :: EmulatorDelegateClients
  , delegateStatesRef :: IORef (Map EmulatorDelegate DelegateState)
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
runEmulatorUsingDockerCompose action =
  runningThreeNodesDockerComposeHydra $ \clients -> do
    intialStatesRef <-
      newIORef $
        Map.fromList [(name, initialState) | name <- allDelegates]
    let context =
          MkEmulatorContext
            { clients = clients
            , delegateStatesRef = intialStatesRef
            }
    flip runReaderT context $ unDelegatesClusterEmulator action

runCompositeForDelegate ::
  forall x.
  EmulatorDelegate ->
  StateT DelegateState CompositeRunner x ->
  DelegatesClusterEmulator x
runCompositeForDelegate name action = do
  MkEmulatorContext {clients, delegateStatesRef} <- ask

  -- Get state
  states <- liftIO $ readIORef delegateStatesRef
  let state = (Map.!) states name

  -- Run
  let (hydraClient, actor) = (Map.!) clients name
  (result, newState) <-
    liftIO $ do
      context <- executeRunnerWithLocalNode $ withActor actor $ do
        l1Context <- ask
        executeHydraRunnerFakingParams hydraClient $ do
          hydraContext <- ask
          return $ MkCompositeExecutionContext {hydraContext, l1Context}
      executeCompositeRunner context (runStateT action state)

  -- Update state
  liftIO $ writeIORef delegateStatesRef $ Map.insert name newState states

  -- Return
  return result

runCompositeForAllDelegates ::
  forall x.
  StateT DelegateState CompositeRunner x ->
  DelegatesClusterEmulator [x]
runCompositeForAllDelegates action =
  mapM (`runCompositeForDelegate` action) allDelegates
