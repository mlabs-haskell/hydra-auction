module HydraAuctionUtils.Delegate.Server (
  queueHydraEvents,
  runDelegateLogicSteps,
) where

import HydraAuctionUtils.Prelude

import Control.Concurrent.STM (
  TChan,
  TQueue,
  flushTQueue,
  tryReadTQueue,
  writeTChan,
  writeTQueue,
 )

import HydraAuctionUtils.Delegate.Interface
import HydraAuctionUtils.Delegate.Logic
import HydraAuctionUtils.Hydra.Monad (waitForHydraEvent)
import HydraAuctionUtils.Hydra.Runner (HydraRunner)
import HydraAuctionUtils.WebSockets.Client (
  AwaitedOutput (..),
  ProtocolClientFor,
 )
import HydraAuctionUtils.WebSockets.ClientId (
  ClientId,
  ClientResponseScope (..),
 )
import HydraAuctionUtils.WebSockets.Protocol (withClient)

queueHydraEvents ::
  forall void any.
  TQueue (DelegateEvent any) ->
  HydraRunner void
queueHydraEvents delegateEventQueue = forever $ do
  mEvent <- waitForHydraEvent Any
  case mEvent of
    Just event ->
      liftIO $
        atomically $
          writeTQueue delegateEventQueue $
            HydraEvent event
    Nothing ->
      return ()
      -- liftIO $ putStrLn "Not recieved Hydra events for waiting time"

{- | create a delegate runner that will listen for incoming events
     and output outgoing events accordingly
-}
runDelegateLogicSteps ::
  forall delegateProtocol client void.
  ( DelegateLogic delegateProtocol
  , ProtocolClientFor (DelegatePlatformProtocol delegateProtocol) client
  ) =>
  -- | the time in milliseconds that the runner sleeps between acts
  Int ->
  client ->
  -- | the queue of incoming messages
  TQueue (DelegateEvent delegateProtocol) ->
  TQueue (ClientId, FrontendRequest delegateProtocol) ->
  -- | the broadcast queue of outgoing messages (write only)
  TChan (ClientResponseScope, DelegateResponse delegateProtocol) ->
  HydraRunner void
runDelegateLogicSteps
  tick
  platformClient
  eventQueue
  frontendRequestQueue
  broadcast = do
    flip evalStateT initialState $ forever $ do
      -- Any Hydra state changes could affect FrontendRequest execution
      -- That means that we cannot process multiple FrontendRequests
      -- on same run, cuz interleaving Hydra events might affect them.
      -- To prevent that we first process all Hydra events in queue,
      -- and then process not more than one FrontendRequest in a tick.

      processQueueWith
        eventQueue
        ( (withClient platformClient . delegateEventStep)
            -- All delegateEventStep responses should be brodcasted
            >=> return . fmap (Broadcast,)
        )

      mEvent <- liftIO . atomically $ tryReadTQueue frontendRequestQueue
      void $
        traverse
          (performStep delegateFrontendRequestStep)
          mEvent

      -- FIXME: log queues overload and make tick not-static
      liftIO $ threadDelay tick
    where
      performStep ::
        forall req.
        (Show req) =>
        ( req ->
          StateT
            (DelegateState delegateProtocol)
            HydraRunner
            [(ClientResponseScope, DelegateResponse delegateProtocol)]
        ) ->
        req ->
        StateT (DelegateState delegateProtocol) HydraRunner ()
      performStep step event = do
        -- FIXME: use logging
        -- liftIO $ putStrLn $ "Delegate logic event: " <> show event
        response <- step event
        -- liftIO $ putStrLn $ "Delegate logic response: " <> show response
        putInToClientsQueue response
      processQueueWith queue step =
        flushAndTraverseNewElements queue $ performStep step
      putInToClientsQueue responses = liftIO $ do
        atomically $
          void $
            traverse (writeTChan broadcast) responses
      flushAndTraverseNewElements queue handler = do
        elements <- liftIO . atomically $ flushTQueue queue
        void $ traverse handler elements
