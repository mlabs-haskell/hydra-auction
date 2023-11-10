module HydraAuctionUtils.WebSockets.Client (
  RealProtocolClient (..),
  FakeProtocolClient (..),
  FakeProtocolClientState (..),
  AwaitedOutput (..),
  OutputMatcher (..),
  withProtocolClient,
  waitForMatchingOutputH,
  newFakeClient,
  -- Re-export
  ProtocolClient (..),
  ProtocolClientFor,
)
where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports
import Data.Aeson (eitherDecodeStrict, encode)
import Data.Proxy (Proxy (..))
import Network.WebSockets (Connection, receiveData, sendTextData)

-- Hydra imports
import Hydra.Network (Host)

-- HydraAuction imports
import HydraAuctionUtils.Network (withClientForHost)
import HydraAuctionUtils.WebSockets.ClientId (clientIsInScope)
import HydraAuctionUtils.WebSockets.Protocol (
  Protocol (..),
  ProtocolClient (..),
  ProtocolClientFor,
  ProtocolServerLogic (..),
 )

-- Generic functions

data AwaitedOutput protocol
  = Any
  | SpecificOutput (Output protocol)
  | SpecificKind (OutputKind protocol)
  | CustomMatcher (OutputMatcher protocol)

deriving stock instance Protocol protocol => Show (AwaitedOutput protocol)

newtype OutputMatcher protocol = OutputMatcher (Output protocol -> Bool)

instance Show (OutputMatcher x) where
  show (OutputMatcher _) = "EventMatcher <some HydraEvent predicate>"

waitForMatchingOutputH ::
  (ProtocolClientFor protocol client, MonadIO m) =>
  client ->
  AwaitedOutput protocol ->
  m (Output protocol)
waitForMatchingOutputH client awaitedSpec = do
  -- FIXME: log awaiting and getting
  mOutput <- receiveOutputH client
  putStrLn $ "Seen output while awaiting: " <> show mOutput
  case mOutput of
    Just output
      | matchingPredicate output ->
          return output
    _ -> waitForMatchingOutputH client awaitedSpec
  where
    matchingPredicate event = case awaitedSpec of
      Any -> True
      SpecificKind expectedKind ->
        getOutputKind event == expectedKind
      SpecificOutput expectedEvent -> event == expectedEvent
      CustomMatcher (OutputMatcher customMatcher) -> customMatcher event

-- RealProtocolClient

data RealProtocolClient protocol = MkRealProtocolClient
  { host :: Host
  , connection :: Connection
  }

instance Protocol protocol => ProtocolClient (RealProtocolClient protocol) where
  type ClientFor (RealProtocolClient protocol) = protocol
  sendInputH handle command =
    liftIO $ sendTextData (connection handle) $ encode command
  receiveOutputH (MkRealProtocolClient {connection}) = do
    wsData <- liftIO $ receiveData connection
    return $ hush $ eitherDecodeStrict wsData

withProtocolClient ::
  forall protocol x m.
  (Protocol protocol, MonadBaseControl IO m) =>
  Host ->
  ConnectionConfig protocol ->
  (RealProtocolClient protocol -> m x) ->
  m x
withProtocolClient host config action =
  withClientForHost
    host
    (configToConnectionPath (Proxy :: Proxy protocol) config)
    (action . MkRealProtocolClient host)

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
  forall implementation.
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
      liftIO $ takeMVar stateVar
    (scopedOutputs, newServerState) <-
      runStateT (implementation (fakeClientId, input)) currentServerState
    let newReceivedOutputs =
          map snd $
            filter (clientIsInScope fakeClientId . fst) scopedOutputs
    liftIO $
      putMVar stateVar $
        MkFakeProtocolState
          { currentServerState = newServerState :: State implementation
          , receivedOutputs = newReceivedOutputs <> receivedOutputs
          }

  receiveOutputH (MkFakeProtocolClient stateVar) = do
    fakeState <- liftIO $ takeMVar stateVar
    case receivedOutputs fakeState of
      first : rest -> do
        liftIO $ putMVar stateVar $ fakeState {receivedOutputs = rest}
        return (Just first)
      [] -> return Nothing
