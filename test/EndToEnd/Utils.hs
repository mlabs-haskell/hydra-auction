module EndToEnd.Utils (mkAssertion) where

-- Prelude imports
import Hydra.Prelude hiding (threadDelay)

-- Haskell test imports
import Test.Hydra.Prelude (failAfter)
import Test.Tasty.HUnit (Assertion)

-- Hydra auction imports
import HydraAuction.Runner (Runner, executeTestRunner)

mkAssertion :: Runner () -> Assertion
mkAssertion = failAfter 60 . executeTestRunner
