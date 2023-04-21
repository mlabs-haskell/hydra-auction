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
import Hydra.Network (IP, PortNumber)

-- Hydra auction imports
import HydraAuction.Delegate (ClientId, ClientResponseScope)
import HydraAuction.Delegate.Interface (DelegateResponse, FrontendRequest)
import HydraAuction.Types (AuctionTerms)
import HydraAuctionUtils.Fixture (Actor)
import HydraAuctionUtils.Tracing (TracerT)

-- | The config for the delegate server
data DelegateServerConfig = DelegateServerConfig
  { host :: IP
  -- ^ the host of the delegate server
  , port :: PortNumber
  -- ^ the port number the delegate server receives input at
  , tick :: Int
  -- ^ the amount of milliseconds, the thread should wait
  , ping :: Int
  -- ^ the amount of seconds between pings for the client thread
  , l1Actor :: Actor
  , hydraServerNumber :: Int
  }

-- | Representation of the Delegate Server's log
data DelegateServerLog
  = Started PortNumber
  | GotFrontendConnected ClientId
  | GotFrontendRequest FrontendRequest
  | ProducedDelegateResponse (ClientResponseScope, DelegateResponse)
  | ThreadEvent ThreadEvent ThreadSort
  | QueueAuctionPhaseEvent QueueAuctionPhaseEvent
  deriving stock (Eq, Show, Generic)

instance Pretty DelegateServerLog where
  pretty = \case
    Started port -> "Started Server at Port" <+> viaShow port
    GotFrontendConnected clientId ->
      "Frontend with clientId " <> viaShow clientId <> "connected to Server"
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
