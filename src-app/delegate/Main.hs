module Main (main) where

-- Prelude imports

import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM (
  TChan,
  TQueue,
  dupTChan,
  newTChan,
  newTQueue,
  readTChan,
  writeTQueue,
 )
import Control.Tracer (Tracer, stdoutTracer)
import Prettyprinter (Pretty (pretty))

-- HydraAuction imports

import DelegateServer.Parsers (delegateConfigParser)
import HydraAuction.Delegate (DelegateEvent (..))
import HydraAuction.Delegate.Interface (
  CustomEvent (..),
  DelegateProtocol,
  DelegateResponse (..),
 )
import HydraAuction.Delegate.Server (
  DelegateServerConfig (..),
  DelegateServerLog (..),
  DelegateTracerT,
  QueueAuctionPhaseEvent (ReceivedAuctionSet),
  ThreadEvent (ThreadCancelled, ThreadStarted),
  ThreadSort (..),
 )
import HydraAuction.OnChain.Common (secondsLeftInInterval, stageToInterval)
import HydraAuction.Tx.Common (currentAuctionStage)
import HydraAuction.Types (AuctionTerms)
import HydraAuctionUtils.Delegate.Server (
  queueHydraEvents,
  runDelegateLogicSteps,
 )
import HydraAuctionUtils.Hydra.Interface (
  HydraConnectionConfig (..),
 )
import HydraAuctionUtils.Hydra.Runner (executeHydraRunnerFakingParams)
import HydraAuctionUtils.L1.Runner (executeL1RunnerWithNodeAs)
import HydraAuctionUtils.Parsers (execParserForCliArgs)
import HydraAuctionUtils.Server.Client (
  AwaitedOutput (..),
  withProtocolClient,
 )
import HydraAuctionUtils.Server.ClientId (
  ClientId,
  ClientResponseScope (Broadcast),
  clientIsInScope,
 )
import HydraAuctionUtils.Server.Protocol (ProtocolClientFor, withClient)
import HydraAuctionUtils.Time (currentPlutusPOSIXTime)
import HydraAuctionUtils.Tracing (
  MonadTracer (trace),
  askTracer,
  runWithTracer',
 )
import HydraAuctionUtils.WebSockets.Client (
  withProtocolClient,
 )
import HydraAuctionUtils.WebSockets.ClientId (
  ClientResponseScope (Broadcast),
 )
import HydraAuctionUtils.WebSockets.Server (
  ServerQueues (..),
  runWebsocketsServer,
 )

-- | run a delegate server
runDelegateServer ::
  HasCallStack =>
  -- | the configuration of the delegate server
  DelegateServerConfig ->
  DelegateTracerT IO ()
runDelegateServer conf = do
  tracer <- askTracer

  eventQueue <- liftIO . atomically $ do
    q <- newTQueue
    writeTQueue q Start
    pure q
  frontendRequestQueue <- liftIO . atomically $ newTQueue
  toClientsChannel <- liftIO . atomically $ newTChan

  let workerAction threadSort =
        withStartStopTrace threadSort $
          case threadSort of
            WebsocketThread ->
              let queues =
                    MkServerQueues
                      { clientInputs = frontendRequestQueue
                      , serverOutputs = toClientsChannel
                      }
               in liftIO $ runWebsocketsServer (websocketsHost conf) queues
            DelegateLogicStepsThread ->
              void $
                liftIO $ do
                  withProtocolClient (platformHost conf) () $ \platformClient ->
                    executeHydraRunnerForConfig $
                      runDelegateLogicSteps
                        (tick conf)
                        platformClient
                        eventQueue
                        frontendRequestQueue
                        toClientsChannel
            QueueAuctionStageThread ->
              mbQueueAuctionPhases (tick conf) eventQueue toClientsChannel
            QueueHydraEventsThread ->
              liftIO $
                executeHydraRunnerForConfig $
                  queueHydraEvents eventQueue

  liftIO $
    mapConcurrently_
      (runWithTracer' tracer . workerAction)
      [ WebsocketThread
      , QueueAuctionStageThread
      , QueueHydraEventsThread
      , DelegateLogicStepsThread
      ]
  where
    executeHydraRunnerForConfig action =
      withProtocolClient (hydraNodeHost conf) clientConfig $ \hydraClient -> do
        executeL1RunnerWithNodeAs (cardanoNode conf) (l1Actor conf) $ do
          executeHydraRunnerFakingParams hydraClient action
    clientConfig = MkHydraConnectionConfig {retrieveHistory = True}

-- | start a worker and log it
withStartStopTrace :: ThreadSort -> DelegateTracerT IO () -> DelegateTracerT IO ()
withStartStopTrace threadSort action = do
  trace $ ThreadEvent ThreadStarted threadSort
  action
  -- FIXME: use finally or something like that
  trace $ ThreadEvent ThreadCancelled threadSort

mbQueueAuctionPhases ::
  Int ->
  TQueue (DelegateEvent DelegateProtocol) ->
  TChan (ClientResponseScope, DelegateResponse DelegateProtocol) ->
  DelegateTracerT IO ()
mbQueueAuctionPhases tick delegateEvents toClientsChannel = do
  terms <-
    liftIO $
      awaitForAuctionSetTerms =<< atomically (dupTChan toClientsChannel)
  traceQueueEvent terms
  liftIO $ queueCurrentStageAndWaitForNextLoop terms
  where
    traceQueueEvent :: AuctionTerms -> DelegateTracerT IO ()
    traceQueueEvent = trace . QueueAuctionPhaseEvent . ReceivedAuctionSet

    awaitForAuctionSetTerms ::
      TChan (ClientResponseScope, DelegateResponse DelegateProtocol) -> IO AuctionTerms
    awaitForAuctionSetTerms chan =
      atomically (readTChan chan) >>= \case
        (Broadcast, CustomEventHappened (AuctionSet terms)) -> pure terms
        _ -> do
          threadDelay tick
          awaitForAuctionSetTerms chan

    queueCurrentStageAndWaitForNextLoop :: AuctionTerms -> IO ()
    queueCurrentStageAndWaitForNextLoop terms = do
      currentStage <- currentAuctionStage terms
      atomically $
        writeTQueue delegateEvents $
          CustomEvent $
            AuctionStageStarted terms currentStage
      currentPosixTime <- currentPlutusPOSIXTime
      let mSecsLeft =
            secondsLeftInInterval
              currentPosixTime
              (stageToInterval terms currentStage)
      putStrLn $
        "Current stage is "
          <> show currentStage
          <> " , awaiting "
          <> show mSecsLeft
          <> " until next stage."
      case mSecsLeft of
        Nothing -> pure ()
        Just s -> do
          -- In case of on stage intervals boundaries,
          -- `secsToWait` is 0, which leads to stuttering
          let secsToWait = if s == 0 then 1 else s
          threadDelay $ fromInteger $ secsToWait * 1_000_000
          queueCurrentStageAndWaitForNextLoop terms

main :: HasCallStack => IO ()
main = do
  --- XXX: this prevents logging issues in Docker
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  conf <- execParserForCliArgs delegateConfigParser
  runWithTracer' tracer $ do
    trace $ Started conf
    runDelegateServer conf
  where
    tracer :: Tracer IO DelegateServerLog
    tracer = contramap (show . pretty) stdoutTracer
