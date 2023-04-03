module HydraAuction.Delegate.Server (
  -- * Delegate server types

  -- ** Server config types
  DelegateServerConfig (..),

  -- ** Server log types
  DelegateServerLog (..),
  DelegateError (..),
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
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.WebSockets (PendingConnection)
import Prettyprinter (Doc, Pretty (pretty), indent, line, viaShow, (<+>))

-- Cardano imports
import Hydra.Network (IP, PortNumber)

-- Hydra auction imports
import HydraAuction.Delegate.Interface (DelegateResponse, FrontendRequest)
import HydraAuction.Types (AuctionTerms)
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
  }

-- | Representation of the Delegate Server's log
data DelegateServerLog
  = Started PortNumber
  | FrontendConnected
  | FrontendInput FrontendRequest
  | DelegateOutput DelegateResponse
  | DelegateError DelegateError
  | ThreadEvent ThreadEvent ThreadSort
  | QueueAuctionPhaseEvent QueueAuctionPhaseEvent
  deriving stock (Eq, Show, Generic)

instance Pretty DelegateServerLog where
  pretty = \case
    Started port -> "Started Server at Port" <+> viaShow port
    FrontendConnected -> "Frontend connected to Server"
    DelegateOutput out -> "Delegate output" <> extraInfo (viaShow out)
    FrontendInput inp -> "Frontend input" <> extraInfo (viaShow inp)
    DelegateError err -> "Delegate runner error occured" <> extraInfo (pretty err)
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
  | DelegateRunnerThread
  | QueueAuctionStageThread
  deriving stock (Eq, Ord, Show)
  deriving (Pretty) via ViaShow ThreadSort

-- | an event happening in the queueAuctionPhase thread
newtype QueueAuctionPhaseEvent
  = ReceivedAuctionSet AuctionTerms
  deriving stock (Eq, Ord, Show)

instance Pretty QueueAuctionPhaseEvent where
  pretty = \case
    ReceivedAuctionSet terms -> "Received an AuctionSet" <> extraInfo (viaShow terms)

{- | the error that can be thrown by the delegate server, before entering the Delegate
   transformer
-}
newtype DelegateError = FrontendNoParse String
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty DelegateError where
  pretty = \case
    FrontendNoParse err -> "Could not parse the input provided by the frontend" <> extraInfo (pretty err)

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
