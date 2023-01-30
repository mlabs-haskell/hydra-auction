module Main (main) where

import Test.Tasty qualified as Tasty

import Prelude

-- import EndToEnd.Spec (headTests)
import OnChain.Spec (onChainTests)

main :: IO ()
main = do
  -- headSpec <- headTests
  onChainSpec <- onChainTests
  Tasty.defaultMain $
    Tasty.testGroup
      "Hydra-auction"
      -- [ headSpec
      [ onChainSpec
      ]
