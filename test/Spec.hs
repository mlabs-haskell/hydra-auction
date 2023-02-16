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

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain testSuite

testSuite :: TestTree
testSuite =
  testGroup
    "Hydra auction tests"
    [ Ledger.testSuite
    , Hydra.testSuite
    ]
