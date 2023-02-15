module EndToEnd.Utils (mkAssertion, waitUntil) where

-- Prelude imports
import Hydra.Prelude hiding (threadDelay)

-- Haskell imports
import Control.Concurrent (threadDelay)

-- Haskell test imports
import Test.Hydra.Prelude (failAfter)
import Test.Tasty.HUnit (Assertion)

-- Plutus imports
import Plutus.V1.Ledger.Time (POSIXTime (..))

-- Hydra imports
import CardanoClient (queryTip)
import CardanoNode (RunningNode (..))
import Hydra.Cardano.Api (ChainPoint (..), SlotNo (..))
import Hydra.Logging (showLogsOnFailure)

-- Hydra auction imports
import HydraAuction.Runner (
  ExecutionContext (..),
  Runner,
  executeTestRunner,
 )
import HydraAuction.Tx.Common (toSlotNo)

mkAssertion :: Runner () -> Assertion
mkAssertion runner =
  showLogsOnFailure $
    \tracer' ->
      timeoutTest $ local (\ctx -> ctx {tracer = tracer'}) runner
  where
    timeoutTest :: Runner () -> IO ()
    timeoutTest runner' =
      failAfter 60 $ executeTestRunner runner'

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
