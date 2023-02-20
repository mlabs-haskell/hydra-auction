module HydraAuction.Runner.Time (
  waitUntil,
  currentSlot,
) where

-- Prelude imports
import Hydra.Prelude hiding (threadDelay)

-- Haskell imports
import Control.Concurrent (threadDelay)

-- Plutus imports
import Plutus.V1.Ledger.Time (POSIXTime (..))

-- Hydra imports
import CardanoClient (queryTip)
import CardanoNode (RunningNode (..))
import Hydra.Cardano.Api (ChainPoint (..), SlotNo (..))

-- Hydra auction imports
import HydraAuction.Runner (ExecutionContext (..), Runner)
import HydraAuction.Tx.Common (toSlotNo)

waitUntil :: POSIXTime -> Runner ()
waitUntil time = do
  slotToWait <- toSlotNo time
  waitUntilSlot slotToWait

waitUntilSlot :: SlotNo -> Runner ()
waitUntilSlot awaitedSlot = do
  currentSlot' <- currentSlot
  when (currentSlot' < awaitedSlot) $ do
    liftIO $ threadDelay 1_000
    waitUntilSlot awaitedSlot

currentSlot :: Runner SlotNo
currentSlot = do
  MkExecutionContext {node} <- ask
  tip <- liftIO $ queryTip (networkId node) (nodeSocket node)
  return $
    case tip of
      ChainPointAtGenesis -> SlotNo 0
      ChainPoint slotNo _ -> slotNo
