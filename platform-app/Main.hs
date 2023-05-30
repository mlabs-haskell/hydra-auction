-- Prelude imports

import Prelude

-- Haskell imports

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (
  TChan,
  TQueue,
  atomically,
  newTChan,
  newTQueue,
  tryReadTQueue,
  writeTChan,
 )
import Control.Monad (forever, void)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans (MonadIO (..))
import Data.Foldable (traverse_)

-- HydraAuction imports

import HydraAuction.Platform.Interface (SomeClientInput, SomeServerOutput)
import HydraAuction.Platform.Storage (initialStorage, processClientInput)
import HydraAuctionUtils.Server.ClientId (
  ClientId,
  ClientResponseScope (..),
 )
import HydraAuctionUtils.Server.Websockets (runWebsocketsServer)

runPlatformEventReaction ::
  forall void.
  -- | the time in milliseconds that the runner sleeps between acts
  Int ->
  TQueue (ClientId, SomeClientInput) ->
  -- | the broadcast queue of outgoing messages (write only)
  TChan (ClientResponseScope, SomeServerOutput) ->
  IO void
runPlatformEventReaction
  tick
  clientInputQueue
  broadcast = do
    flip evalStateT initialStorage $ forever $ do
      mEvent <- liftIO . atomically $ tryReadTQueue clientInputQueue
      traverse_
        performStep
        mEvent
      -- FIXME: log queues overload and make tick not-static
      liftIO $ threadDelay tick
    where
      performStep (clientId, event) = do
        -- FIXME: use logging
        liftIO $ putStrLn $ "Platform logic input: " <> show event
        response <- processClientInput event
        liftIO $ putStrLn $ "Platform logic output: " <> show response
        putInToClientsQueue [(PerClient clientId, response)]
      putInToClientsQueue responses =
        liftIO $
          atomically $
            traverse_ (writeTChan broadcast) responses

runPlatformServer :: IO ()
runPlatformServer = do
  clientInputQueue <- liftIO . atomically $ newTQueue
  toClientsChannel <- liftIO . atomically $ newTChan

  void $
    concurrently
      (runWebsocketsServer 8010 clientInputQueue toClientsChannel)
      (runPlatformEventReaction 1_000 clientInputQueue toClientsChannel)

main :: IO ()
main = runPlatformServer
