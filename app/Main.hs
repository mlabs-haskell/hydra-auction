module Main (main) where

import Prelude

import CliActions (handleCliAction)
import CliParsers (getCliAction)

main :: IO ()
main = do
  cliAction <- getCliAction
  handleCliAction cliAction
