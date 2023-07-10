{-# OPTIONS_GHC -Wno-orphans #-}

module HydraAuction.Delegate.Server (
  -- * Delegate server types

  -- ** Server config types
  DelegateServerConfig (..),

  -- ** Server log types
  DelegateServerLog (..),
  ThreadSort (..),
  ThreadEvent (..),
  QueueAuctionPhaseEvent (..),

  -- ** delegate tracing
  DelegateTracerT,

  -- * wai extras
  ServerAppT,

  -- * pretty extras
  ViaShow (..),
  extraInfo,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import GHC.Generics (Generic)
import Network.WebSockets (PendingConnection)
import Prettyprinter (Doc, Pretty (pretty), indent, line, viaShow, (<+>))

-- Cardano imports

import CardanoNode (RunningNode (..))
import Hydra.Network (Host)

-- Hydra auction imports

import HydraAuction.Delegate.Interface (
  DelegateProtocol,
  DelegateResponse,
  FrontendRequest,
 )
import HydraAuction.Types (AuctionTerms)
import HydraAuctionUtils.Fixture (Actor)
import HydraAuctionUtils.Tracing (TracerT)
import HydraAuctionUtils.WebSockets.ClientId (ClientId, ClientResponseScope)

deriving stock instance Eq RunningNode
deriving stock instance Show RunningNode

-- | The config for the delegate server
data DelegateServerConfig = DelegateServerConfig
  { websocketsHost :: Host
  , cardanoNode :: RunningNode
  , hydraNodeHost :: Host
  , platformHost :: Host
  , l1Actor :: Actor
  -- ^ should match Hydra node acto
  , tick :: Int
  -- ^ the amount of milliseconds, the event polling threads should wait
  , ping :: Int
  -- ^ the amount of seconds between pings for the client thread
  }
  deriving stock (Eq, Show, Generic)

-- | Representation of the Delegate Server's log
data DelegateServerLog
  = Started DelegateServerConfig
  | GotFrontendConnected ClientId
  | GotFrontendRequest (FrontendRequest DelegateProtocol)
  | ProducedDelegateResponse
      (ClientResponseScope, DelegateResponse DelegateProtocol)
  | ThreadEvent ThreadEvent ThreadSort
  | QueueAuctionPhaseEvent QueueAuctionPhaseEvent
  deriving stock (Eq, Show, Generic)

instance Pretty DelegateServerLog where
  pretty = \case
    Started port -> "Started Server at Port" <+> viaShow port
    GotFrontendConnected clientId ->
      "Frontend with clientId " <> viaShow clientId <> " connected to Server"
    ProducedDelegateResponse out -> "Delegate output" <> extraInfo (viaShow out)
    GotFrontendRequest inp -> "Frontend input" <> extraInfo (viaShow inp)
    ThreadEvent ev info -> "Thread" <+> pretty info <> ":" <> extraInfo (pretty ev)
    QueueAuctionPhaseEvent ev -> "Auction phase queueing" <> extraInfo (pretty ev)

-- | The event type to occur
data ThreadEvent
  = ThreadStarted
  | ThreadCancelled
  deriving stock (Eq, Ord, Show)

instance Pretty ThreadEvent where
  pretty = \case
    ThreadStarted -> "Thread started"
    ThreadCancelled -> "Thread cancelled"

-- | Which specific thrad the event originates from
data ThreadSort
  = WebsocketThread
  | DelegateLogicStepsThread
  | QueueAuctionStageThread
  | QueueHydraEventsThread
  deriving stock (Eq, Ord, Show)
  deriving (Pretty) via ViaShow ThreadSort

-- | an event happening in the queueAuctionPhase thread
newtype QueueAuctionPhaseEvent
  = ReceivedAuctionSet AuctionTerms
  deriving stock (Eq, Ord, Show)

instance Pretty QueueAuctionPhaseEvent where
  pretty = \case
    ReceivedAuctionSet terms -> "Received an AuctionSet" <> extraInfo (viaShow terms)

-- | trace a 'DelegateServerLog'
type DelegateTracerT = TracerT DelegateServerLog

-- | like @ServerApp@ but can be used with a transformer
type ServerAppT m = PendingConnection -> m ()

-- | a newtype for deriving Pretty via Show
newtype ViaShow a = ViaShow {unViaShow :: a}
  deriving stock (Eq, Ord, Generic)

instance Show a => Pretty (ViaShow a) where
  pretty = viaShow . unViaShow

-- | additional information on a log event
extraInfo :: forall ann. Doc ann -> Doc ann
extraInfo = (line <>) . indent 2
