module HydraAuction.Delegate.Server (
  -- * Delegate server types
  DelegateServerConfig (..),
  DelegateServerLog (..),
  DelegateError (..),

  -- ** delegate tracing
  DelegateTracerT,

  -- * wai extras
  ServerAppT,
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
import HydraAuction.Delegate.Tracing (TracerT)

-- | The config for the delegate server
data DelegateServerConfig = DelegateServerConfig
  { dlgt'host :: IP
  -- ^ the host of the delegate server
  , dlgt'port :: PortNumber
  -- ^ the port number the delegate server receives input at
  , dlgt'tick :: Int
  -- ^ the amount of milliseconds, the thread should wait
  , dlgt'ping :: Int
  -- ^ the amount of seconds between pings for the client thread
  }

-- | Representation of the Delegate Server's log
data DelegateServerLog
  = Started PortNumber
  | FrontendConnected
  | FrontendInput FrontendRequest
  | DelegateOutput DelegateResponse
  | DelegateError DelegateError
  deriving stock (Eq, Show, Generic)

instance Pretty DelegateServerLog where
  pretty = \case
    Started port -> "Started Server at Port" <+> viaShow port
    FrontendConnected -> "Frontend connected to Server"
    DelegateOutput out -> "Delegate output" <> extraInfo (viaShow out)
    FrontendInput inp -> "Frontend input" <> extraInfo (viaShow inp)
    DelegateError err -> "Delegate error" <> extraInfo (pretty err)

{- | the error that can be thrown by the delegate server, before entering the Delegate
   transformer
-}
newtype DelegateError = FrontendNoParse String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty DelegateError where
  pretty = \case
    FrontendNoParse err -> "Could not parse the input provided by the frontend" <> extraInfo (pretty err)

-- | additional information on a log event
extraInfo :: forall ann. Doc ann -> Doc ann
extraInfo = (line <>) . indent 2

-- | trace a 'DelegateServerLog'
type DelegateTracerT = TracerT DelegateServerLog

-- | like @ServerApp@ but can be used with a transformer
type ServerAppT m = PendingConnection -> m ()
