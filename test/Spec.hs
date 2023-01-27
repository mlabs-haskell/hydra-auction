module Main (main) where

import Test.Tasty qualified as Tasty

import Prelude

-- import EndToEnd.Spec (headTests)
import EndToEnd.StandingBid.NewBid (testSuite)

main :: IO ()
main = do
  spec <- testSuite
  Tasty.defaultMain $
    Tasty.testGroup
      "Hydra-demo"
      [ spec
      ]
