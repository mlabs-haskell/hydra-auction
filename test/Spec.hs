module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (TestTree, defaultMain, testGroup)

import EndToEnd.Hydra qualified as Hydra
import EndToEnd.Ledger qualified as Ledger

import Prelude

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain testSuite

testSuite :: TestTree
testSuite =
  testGroup
    "Hydra auction tests"
    [ Hydra.testSuite
    , Ledger.testSuite
    ]
