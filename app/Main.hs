{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Hydra.Logging (Verbosity (Quiet, Verbose))
import Prelude

import CardanoNodeDevnet (getCardanoNode)
import CliActions (handleCliAction)
import CliParsers (
  CliInput (MkCliInput, cmd, verbosity),
  getCliInput,
 )
import HydraAuction.Runner (executeRunner, stdoutTracer)

main :: IO ()
main = do
  MkCliInput {..} <- getCliInput

  let hydraVerbosity =
        if verbosity then Verbose "hydra-auction" else Quiet

  node <- getCardanoNode
  tracer <- stdoutTracer hydraVerbosity

  executeRunner tracer node verbosity (handleCliAction cmd)
