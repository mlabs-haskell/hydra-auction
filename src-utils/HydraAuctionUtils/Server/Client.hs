module HydraAuctionUtils.Server.Client (
  RealProtocolClient (..),
  FakeProtocolClient (..),
  FakeProtocolClientState (..),
  newFakeClient,
)
where

-- Prelude imports
import HydraAuctionUtils.Prelude

import Data.Aeson (eitherDecodeStrict, encode)
import HydraAuctionUtils.Server.ClientId (clientIsInScope)
import HydraAuctionUtils.Server.Protocol (
  Protocol (..),
  ProtocolClient (..),
  ProtocolServerLogic (..),
 )
import Network.WebSockets (Connection, receiveData, sendTextData)

-- RealProtocolClient

newtype RealProtocolClient protocol = MkRealProtocolClient
  { connection :: Connection
  }

instance Protocol protocol => ProtocolClient (RealProtocolClient protocol) where
  type ClientFor (RealProtocolClient protocol) = protocol
  sendInputH handle command = sendTextData (connection handle) $ encode command
  receiveOutputH (MkRealProtocolClient connection) = do
    wsData <- receiveData connection
    return $ hush $ eitherDecodeStrict wsData

-- FakeProtocolClient

data FakeProtocolClientState implementation = ProtocolServerLogic implementation =>
  MkFakeProtocolState
  { currentServerState :: State implementation
  , receivedOutputs :: [Output (ImplementationFor implementation)]
  }

data FakeProtocolClient implementation = ProtocolServerLogic implementation =>
  MkFakeProtocolClient
  { stateVar :: MVar (FakeProtocolClientState implementation)
  }

newFakeClient ::
  ProtocolServerLogic implementation =>
  IO (FakeProtocolClient implementation)
newFakeClient = do
  var <- newMVar $ MkFakeProtocolState initialState []
  return $ MkFakeProtocolClient var

instance
  ProtocolServerLogic implementation =>
  ProtocolClient (FakeProtocolClient implementation)
  where
  type ClientFor (FakeProtocolClient implementation) = ImplementationFor implementation
  sendInputH (MkFakeProtocolClient stateVar) input = do
    let fakeClientId = 1
    MkFakeProtocolState {currentServerState, receivedOutputs} <-
      takeMVar stateVar
    (scopedOutputs, newServerState) <-
      runStateT (implementation (fakeClientId, input)) currentServerState
    let newReceivedOutputs =
          map snd $
            filter (clientIsInScope fakeClientId . fst) scopedOutputs
    putMVar stateVar $
      MkFakeProtocolState
        { currentServerState = newServerState :: State implementation
        , receivedOutputs = receivedOutputs <> newReceivedOutputs
        }

  receiveOutputH (MkFakeProtocolClient stateVar) = do
    fakeState <- takeMVar stateVar
    case receivedOutputs fakeState of
      first : rest -> do
        putMVar stateVar $ fakeState {receivedOutputs = rest}
        return (Just first)
      [] -> return Nothing
