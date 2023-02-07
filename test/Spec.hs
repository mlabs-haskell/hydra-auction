module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain)

import EndToEnd.Hydra (testSuite)
import Prelude

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain testSuite
