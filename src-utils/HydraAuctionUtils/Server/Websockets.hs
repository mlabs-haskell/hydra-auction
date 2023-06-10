module HydraAuctionUtils.Server.Websockets (runWebsocketsServer) where

-- Prelude imports

import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (mapConcurrently_, race_)
import Control.Concurrent.STM (
  TChan,
  TQueue,
  atomically,
  dupTChan,
  readTChan,
  writeTQueue,
 )
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Network.WebSockets (
  Connection,
  PendingConnection,
  acceptRequest,
  receiveData,
  runServer,
  sendTextData,
  withPingThread,
 )

-- HydraAuction imports
import HydraAuctionUtils.Server.ClientId (
  ClientId,
  ClientResponseScope (..),
  clientIsInScope,
 )

-- FIXME: return proper logging
websocketsServerConnectionHanler ::
  forall input output.
  (FromJSON input, ToJSON output) =>
  -- | the amount of seconds between pings
  Int ->
  -- | the queue for the client requests
  TQueue (ClientId, input) ->
  -- | the queue for the broadcast
  TChan (ClientResponseScope, output) ->
  PendingConnection ->
  IO ()
websocketsServerConnectionHanler
  pingSecs
  clientInputQueue
  broadcast
  pending = do
    clientIdCounter <- liftIO $ newMVar 0
    let genFreshClientId = freshClientIdGenerator clientIdCounter

    connection <- liftIO $ acceptRequest pending
    clientId <- liftIO genFreshClientId

    liftIO $ do
      toClientsChannelCopy <- atomically $ dupTChan broadcast
      -- From the Documentation of 'withPingThread' in 'Network.Websockets':
      -- This is useful to keep idle connections open through proxies and whatnot.
      -- Many (but not all) proxies have a 60 second default timeout, so based on
      -- that sending a ping every 30 seconds is a good idea.
      withPingThread connection pingSecs (pure ()) $
        race_
          (forever $ receiveAndPutInQueue connection clientId)
          ( forever $
              sendFromChannel connection clientId toClientsChannelCopy
          )
    where
      sendToClient :: forall a. ToJSON a => Connection -> a -> IO ()
      sendToClient connection = sendTextData connection . encode
      receiveAndPutInQueue ::
        Connection ->
        ClientId ->
        IO ()
      receiveAndPutInQueue connection clientId = do
        inp <- liftIO $ receiveData connection
        case eitherDecode @input inp of
          Left _ -> return ()
          Right request -> do
            liftIO . atomically $
              writeTQueue clientInputQueue (clientId, request)
      sendFromChannel ::
        Connection ->
        ClientId ->
        TChan (ClientResponseScope, output) ->
        IO ()
      sendFromChannel connection clientId broadcastCopy = do
        (scope, message) <- atomically (readTChan broadcastCopy)
        when (clientIsInScope clientId scope) $
          sendToClient connection message
      freshClientIdGenerator :: MVar ClientId -> IO ClientId
      freshClientIdGenerator clientCounter = do
        v <- takeMVar clientCounter
        putMVar clientCounter (v + 1)
        return v

runWebsocketsServer ::
  forall input output.
  (FromJSON input, ToJSON output) =>
  Int ->
  -- | the queue for the client requests
  TQueue (ClientId, input) ->
  -- | the queue for the broadcast
  TChan (ClientResponseScope, output) ->
  IO ()
runWebsocketsServer port clientInputQueue toClientsChannel =
  -- FIXME: parametrize with custom config datatype
  runServer "127.0.0.1" port $
    websocketsServerConnectionHanler 30_000 clientInputQueue toClientsChannel
