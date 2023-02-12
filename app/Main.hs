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
import Hydra.Logging (Verbosity (Quiet))
import Prelude

import CliActions (handleCliAction)
import CliParsers (
  Options (MkOptions, cmd, verbosity),
  getOptions,
 )
import HydraAuction.Runner (executeRunner, stdoutTracer)

main :: IO ()
main = do
  MkOptions {..} <- getOptions
  tracer <- stdoutTracer verbosity

  let node =
        RunningNode
          { nodeSocket = "./node.socket"
          , networkId = Testnet $ NetworkMagic 42
          }
      verbose = case verbosity of
        Quiet -> False
        _ -> True

  executeRunner
    tracer
    node
    verbose
    $ handleCliAction cmd
