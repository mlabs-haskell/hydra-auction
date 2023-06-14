module HydraAuctionUtils.Server.Protocol (
  Protocol (..),
  ProtocolClient (..),
  ProtocolServerLogic (..),
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports
import Data.Aeson (FromJSON (..), ToJSON (..))

-- HydraAuction imports
import HydraAuctionUtils.Server.ClientId (ClientId, ClientResponseScope)

type ToFromJSON x = (ToJSON x, FromJSON x)

class (ToFromJSON (Input protocol), ToFromJSON (Output protocol)) => Protocol protocol where
  type Input protocol = input | input -> protocol
  type Output protocol = output | output -> protocol

class Protocol (ClientFor handle) => ProtocolClient handle where
  type ClientFor handle
  sendInputH :: handle -> Input (ClientFor handle) -> IO ()
  receiveOutputH :: handle -> IO (Maybe (Output (ClientFor handle)))

class Protocol (ImplementationFor implementation) => ProtocolServerLogic implementation where
  type ImplementationFor implementation
  type State implementation = state | state -> implementation
  initialState :: State implementation
  implementation ::
    (ClientId, Input (ImplementationFor implementation)) ->
    StateT (State implementation) m [(ClientResponseScope, Output (ImplementationFor implementation))]
