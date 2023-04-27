-- FIXME: should be abstracted on MonadBlockchainParams
module HydraAuctionUtils.L1.Runner.Time (
  waitUntil,
  currentSlot,
) where

-- Prelude imports

import Hydra.Prelude (ask, liftIO, when)
import Prelude

-- Haskell imports
import Control.Concurrent (threadDelay)

-- Plutus imports
import Plutus.V1.Ledger.Time (POSIXTime (..))

-- Hydra imports
import CardanoClient (queryTip)
import CardanoNode (RunningNode (..))
import Hydra.Cardano.Api (ChainPoint (..), SlotNo (..))

-- Hydra auction imports
import HydraAuctionUtils.L1.Runner (ExecutionContext (..), L1Runner)
import HydraAuctionUtils.Monads (MonadBlockchainParams (toSlotNo))

waitUntil :: POSIXTime -> L1Runner ()
waitUntil time = do
  slotToWait <- toSlotNo time
  waitUntilSlot slotToWait

waitUntilSlot :: SlotNo -> L1Runner ()
waitUntilSlot awaitedSlot = do
  currentSlot' <- currentSlot
  when (currentSlot' < awaitedSlot) $ do
    liftIO $ threadDelay 1_000
    waitUntilSlot awaitedSlot

currentSlot :: L1Runner SlotNo
currentSlot = do
  MkExecutionContext {node} <- ask
  tip <- liftIO $ queryTip (networkId node) (nodeSocket node)
  return $
    case tip of
      ChainPointAtGenesis -> SlotNo 0
      ChainPoint slotNo _ -> slotNo
