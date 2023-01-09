module Main (main) where

import Test.Tasty qualified as Tasty

import Prelude

import EndToEnd.Spec (headTests)

main :: IO ()
main = do
  spec <- headTests
  Tasty.defaultMain $
    Tasty.testGroup
      "Hydra-demo"
      [ spec
      ]