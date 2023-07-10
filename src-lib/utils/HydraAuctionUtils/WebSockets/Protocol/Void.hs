module HydraAuctionUtils.WebSockets.Protocol.Void (
  VoidProtocol,
  VoidProtocolImplementation,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports
import Data.Void (Void)

-- HydraAuction imports
import HydraAuctionUtils.WebSockets.Protocol

data VoidProtocol
data VoidProtocolImplementation

instance Protocol VoidProtocol where
  type Input VoidProtocol = Void
  type Output VoidProtocol = Void
  type OutputKind VoidProtocol = Void
  type ConnectionConfig VoidProtocol = ()

  getOutputKind = \case {}
  configToConnectionPath _ () = ""

instance ProtocolServerLogic VoidProtocolImplementation where
  type ImplementationFor VoidProtocolImplementation = VoidProtocol
  type State VoidProtocolImplementation = ()
  initialState = ()
  implementation _input = return []
