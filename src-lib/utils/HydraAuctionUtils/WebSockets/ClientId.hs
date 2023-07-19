module HydraAuctionUtils.WebSockets.ClientId (ClientId, ClientResponseScope (..), clientIsInScope) where

-- Prelude imports
import HydraAuctionUtils.Prelude

type ClientId = Int

data ClientResponseScope
  = Broadcast
  | PerClient ClientId
  deriving stock (Eq, Show)

clientIsInScope :: ClientId -> ClientResponseScope -> Bool
clientIsInScope clientId scope = case scope of
  Broadcast -> True
  PerClient expectedClientId -> clientId == expectedClientId
