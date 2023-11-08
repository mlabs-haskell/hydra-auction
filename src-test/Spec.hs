module Main (main) where

-- Prelude imports
import Prelude

-- Haskell imports
import GHC.IO.Encoding (setLocaleEncoding, utf8)

-- Haskell test imports
import Test.Tasty (TestTree, defaultMain, testGroup)

-- Hydra auction test imports

import EndToEnd.CLI qualified as CLI
import EndToEnd.Ledger.Auction qualified as Ledger.Auction
import EndToEnd.Ledger.BidDeposit qualified as Ledger.BidDeposit
import EndToEnd.Ledger.L2 qualified as LedgerL2
import EndToEnd.Troubleshooting.L2 qualified as LedgerL2Troubleshooting
import Unit.Common qualified as UnitCommon

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain testSuite

testSuite :: TestTree
testSuite =
  testGroup
    "main"
    [ Ledger.Auction.testSuite
    , Ledger.BidDeposit.testSuite
    , LedgerL2.testSuite
    , UnitCommon.testSuite
    , CLI.testSuite
    , LedgerL2Troubleshooting.testSuite
    ]
