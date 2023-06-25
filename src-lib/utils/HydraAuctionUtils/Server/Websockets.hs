module HydraAuctionUtils.Server.Websockets (
  ServerQueues (..),
  runWebsocketsServer,
) where

-- Prelude imports

import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (
  TChan,
  TQueue,
  dupTChan,
  readTChan,
  writeTQueue,
 )
import Data.Aeson (eitherDecode, encode)
import Data.Text qualified as Text
import Network.WebSockets (
  Connection,
  PendingConnection,
  acceptRequest,
  receiveData,
  runServer,
  sendTextData,
  withPingThread,
 )

-- Hydra imports
import Hydra.Network (Host (..))

-- HydraAuction imports
import HydraAuctionUtils.Server.ClientId (
  ClientId,
  ClientResponseScope (..),
  clientIsInScope,
 )
import HydraAuctionUtils.Server.Protocol (Protocol (..))

data ServerQueues protocol = MkServerQueues
  { clientInputs :: TQueue (ClientId, Input protocol)
  -- ^ the queue where WS server thread puts client inputs
  , serverOutputs :: TChan (ClientResponseScope, Output protocol)
  -- ^ the queue where WS server thread gets outputs created
  -- by some another thread with internal logic
  }

-- FIXME: return proper logging
websocketsServerConnectionHanler ::
  forall protocol.
  Protocol protocol =>
  -- | the amount of seconds between pings
  Int ->
  ServerQueues protocol ->
  IO ClientId ->
  PendingConnection ->
  IO ()
websocketsServerConnectionHanler
  pingSecs
  clientQueues
  genFreshClientId
  pending =
    do
      connection <- acceptRequest pending
      clientId <- genFreshClientId
      toClientsChannelCopy <- atomically $ dupTChan (serverOutputs clientQueues)
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
      sendToClient connection = sendTextData connection . encode
      receiveAndPutInQueue ::
        Connection ->
        ClientId ->
        IO ()

      receiveAndPutInQueue connection clientId = do
        inp <- liftIO $ receiveData connection
        case eitherDecode inp of
          Left _ -> return ()
          Right request -> do
            liftIO . atomically $
              writeTQueue (clientInputs clientQueues) (clientId, request)
      sendFromChannel ::
        Connection ->
        ClientId ->
        TChan (ClientResponseScope, Output protocol) ->
        IO ()
      sendFromChannel connection clientId broadcastCopy = do
        (scope, message) <- atomically (readTChan broadcastCopy)
        when (clientIsInScope clientId scope) $
          sendToClient connection message

runWebsocketsServer ::
  forall protocol.
  Protocol protocol =>
  Host ->
  ServerQueues protocol ->
  IO ()
runWebsocketsServer host queues = do
  clientIdCounter <- liftIO $ newMVar 0
  -- FIXME: parametrize with custom config datatype
  runServer (Text.unpack $ hostname host) (fromIntegral $ port host) $
    websocketsServerConnectionHanler
      30_000
      queues
      (freshClientIdGenerator clientIdCounter)
  where
    freshClientIdGenerator :: MVar ClientId -> IO ClientId
    freshClientIdGenerator clientCounter = do
      v <- takeMVar clientCounter
      putMVar clientCounter (v + 1)
      return v
