module EndToEnd.Utils (mkAssertion) where

import Hydra.Logging (showLogsOnFailure)
import Hydra.Prelude (IO, MonadReader (local), ($), (.))
import Test.Hydra.Prelude (failAfter)
import Test.Tasty.HUnit (Assertion)

import HydraAuction.Runner (
  ExecutionContext (tracer),
  Runner,
  tmpStateDirectory,
 )

mkAssertion :: Runner () -> Assertion
mkAssertion runner =
  showLogsOnFailure $
    \tracer' ->
      timeoutTest $ local (\ctx -> ctx {tracer = tracer'}) runner
  where
    timeoutTest :: Runner () -> IO ()
    timeoutTest =
      failAfter 60
        . tmpStateDirectory "end-to-end-cardano-node"
