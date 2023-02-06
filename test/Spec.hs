module Main (main) where

import Test.Tasty qualified as Tasty

import Prelude

import EndToEnd.Spec (tests)

main :: IO ()
main = do
  spec <- tests
  Tasty.defaultMain $
    Tasty.testGroup
      "Hydra-demo"
      [ spec
      ]
