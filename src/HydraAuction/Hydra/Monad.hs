module HydraAuction.Hydra.Monad (
  MonadHydra (..),
  AwaitedHydraEvent (..),
  waitForHydraEvent,
  sendCommandAndWaitFor,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import GHC.Natural (Natural)

-- HydraAuction imports
import HydraAuction.Hydra.Interface (
  HydraCommand,
  HydraEvent,
  HydraEventKind,
 )

data AwaitedHydraEvent = Any | SpecificKind HydraEventKind

class Monad m => MonadHydra m where
  sendCommand :: HydraCommand -> m ()
  waitForHydraEvent' :: Natural -> AwaitedHydraEvent -> m HydraEvent

defaultTimeout :: Natural
defaultTimeout = 30

waitForHydraEvent :: MonadHydra m => AwaitedHydraEvent -> m HydraEvent
waitForHydraEvent = waitForHydraEvent' defaultTimeout

sendCommandAndWaitFor :: MonadHydra m => AwaitedHydraEvent -> HydraCommand -> m HydraEvent
sendCommandAndWaitFor awaitedSpec command = do
  sendCommand command
  waitForHydraEvent awaitedSpec
