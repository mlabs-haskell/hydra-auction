{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Cardano.Api (NetworkId (..))
import CardanoNode (
  RunningNode (
    RunningNode,
    networkId,
    nodeSocket
  ),
  withCardanoNodeDevnet,
 )
import Hydra.Cardano.Api (NetworkMagic (NetworkMagic))
import Prelude

import Hydra.Logging (Verbosity (Quiet), contramap)
import HydraAuction.Runner (executeRunner, stdoutTracer)
import HydraNode (
  EndToEndLog (
    FromCardanoNode
  ),
 )

import CliActions (handleCliAction)
import CliParsers (
  Command (RunCardanoNode),
  Options (MkOptions, cmd, verbosity),
  getOptions,
 )

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

  case cmd of
    RunCardanoNode -> do
      putStrLn "Running cardano-node"
      withCardanoNodeDevnet
        (contramap FromCardanoNode tracer)
        "."
        $ \_ ->
          error "Not implemented: RunCardanoNode"
    _ ->
      executeRunner
        tracer
        node
        verbose
        $ handleCliAction cmd
