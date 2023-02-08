module EndToEnd.Utils (mkAssertion) where

import Hydra.Logging (showLogsOnFailure)
import Hydra.Prelude (IO, MonadReader (local), ($))
import Test.Hydra.Prelude (failAfter)
import Test.Tasty.HUnit (Assertion)

import HydraAuction.Runner (
  ExecutionContext (tracer),
  Runner,
  executeTestRunner,
 )

mkAssertion :: Runner () -> Assertion
mkAssertion runner =
  showLogsOnFailure $
    \tracer' ->
      timeoutTest $ local (\ctx -> ctx {tracer = tracer'}) runner
  where
    timeoutTest :: Runner () -> IO ()
    timeoutTest runner' =
      failAfter 60 $ executeTestRunner runner'
