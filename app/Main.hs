{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Cardano.Api (NetworkId (..))
import CardanoNode (
  RunningNode (
    RunningNode,
    networkId,
    nodeSocket
  ),
 )
import Hydra.Cardano.Api (NetworkMagic (NetworkMagic))
import Hydra.Logging (Verbosity (Quiet, Verbose))
import Prelude

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
      node =
        RunningNode
          { nodeSocket = "./node.socket"
          , networkId = Testnet $ NetworkMagic 42
          }

  tracer <- stdoutTracer hydraVerbosity

  executeRunner tracer node verbosity (handleCliAction cmd)
