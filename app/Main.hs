module Main (main) where

-- Prelude imports
import Prelude

-- Hydra imports
import Hydra.Logging (Verbosity (Quiet, Verbose))

-- Hydra auction imports
import HydraAuction.Runner (executeRunner, stdoutTracer)

-- Hydra auction CLI imports
import CLI.Actions (handleCliAction)
import CLI.CardanoNode (getCardanoNode)
import CLI.Parsers (
  CliInput (MkCliInput, cmd, verbosity),
  getCliInput,
 )

main :: IO ()
main = do
  MkCliInput {verbosity, cmd} <- getCliInput

  let hydraVerbosity =
        if verbosity then Verbose "hydra-auction" else Quiet

  node <- getCardanoNode
  tracer <- stdoutTracer hydraVerbosity

  executeRunner tracer node verbosity (handleCliAction cmd)
