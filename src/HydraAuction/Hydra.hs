module HydraAuction.Hydra (
  sendCommandAndWait, module HydraMonad) where

-- Prelude imports
import Prelude

-- HydraAuction imports
import HydraAuction.Hydra.Monad qualified as HydraMonad
import HydraAuction.Hydra.Monad (MonadHydra (..))
import HydraAuction.Hydra.Interface (HydraCommand (..), HydraEvent (..))

sendCommandAndWait :: MonadHydra m => HydraCommand -> m HydraEvent
sendCommandAndWait command = do
  sendCommand command
  waitForNewEvent 5

