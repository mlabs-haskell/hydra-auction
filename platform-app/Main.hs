-- Prelude imports

import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (
  atomically,
  newTChan,
  newTQueue,
  tryReadTQueue,
  writeTChan,
 )
import Data.Foldable (traverse_)

-- HydraAuction imports

import HydraAuction.Platform.Interface (PlatformProtocol)
import HydraAuction.Platform.Storage (initialStorage, processClientInput)
import HydraAuctionUtils.Server.ClientId (
  ClientResponseScope (..),
 )
import HydraAuctionUtils.Server.Websockets (
  ServerQueues (..),
  runWebsocketsServer,
 )

runPlatformEventReaction ::
  forall void.
  -- | the time in milliseconds that the runner sleeps between acts
  Int ->
  ServerQueues PlatformProtocol ->
  IO void
runPlatformEventReaction
  tick
  queues = do
    flip evalStateT initialStorage $ forever $ do
      mEvent <- liftIO . atomically $ tryReadTQueue (clientInputs queues)
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
            traverse_ (writeTChan (serverOutputs queues)) responses

runPlatformServer :: IO ()
runPlatformServer = do
  clientInputQueue <- liftIO . atomically $ newTQueue
  toClientsChannel <- liftIO . atomically $ newTChan

  let queues =
        MkServerQueues
          { clientInputs = clientInputQueue
          , serverOutputs = toClientsChannel
          }

  void $
    concurrently
      (runWebsocketsServer 8010 queues)
      (runPlatformEventReaction 1_000 queues)

main :: IO ()
main = runPlatformServer
