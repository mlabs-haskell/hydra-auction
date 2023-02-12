module EndToEnd.Utils (mkAssertion, waitUntil) where

import Hydra.Prelude

import Hydra.Logging (showLogsOnFailure)
import Plutus.V1.Ledger.Time (POSIXTime (..))
import Test.Hydra.Prelude (failAfter)
import Test.Tasty.HUnit (Assertion)

import HydraAuction.Runner (
  ExecutionContext (tracer),
  Runner,
  executeTestRunner,
 )
import HydraAuction.Tx.Common (currentTimeSeconds)

mkAssertion :: Runner () -> Assertion
mkAssertion runner =
  showLogsOnFailure $
    \tracer' ->
      timeoutTest $ local (\ctx -> ctx {tracer = tracer'}) runner
  where
    timeoutTest :: Runner () -> IO ()
    timeoutTest runner' =
      failAfter 60 $ executeTestRunner runner'

waitUntil :: POSIXTime -> IO ()
waitUntil time =
  whenM (not <$> alreadyHappened) $ do
    threadDelay 1
    waitUntil time
  where
    alreadyHappened = do
      currentTimeSeconds' <- currentTimeSeconds
      return $ getPOSIXTime time <= currentTimeSeconds' * 1000
