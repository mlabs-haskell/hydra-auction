module Main (main) where

-- Prelude imports
import Prelude

-- Haskell imports
import GHC.IO.Encoding (setLocaleEncoding, utf8)

-- Haskell test imports
import Test.Tasty (TestTree, defaultMain, testGroup)

-- Hydra auction test imports
import EndToEnd.Hydra qualified as Hydra
import EndToEnd.Ledger qualified as Ledger
import Unit.Common qualified as UnitCommon
import EndToEnd.LedgerL2 qualified as LedgerL2

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain testSuite

testSuite :: TestTree
testSuite =
  testGroup
    "hydra-auction"
    [ Ledger.testSuite
    , LedgerL2.testSuite
    , Hydra.testSuite
    , UnitCommon.testSuite
    ]
