module Main (main) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (void, when)

-- Hydra imports

import Hydra.Logging (Verbosity (Quiet, Verbose))
import Hydra.Prelude (contramap, liftIO)

-- Hydra auction imports

import HydraAuction.Fixture (Actor (..))
import HydraAuction.Runner (
  ExecutionContext (..),
  HydraAuctionLog (FromHydra),
  Runner,
  executeRunner,
  stdoutOrNullTracer,
 )

-- Hydra auction CLI imports
import CLI.Actions (handleCliAction)
import CLI.CardanoNode (getCardanoNode, runCardanoNode)
import CLI.Parsers (
  CliInput (MkCliInput, cliActor, cliVerbosity),
  getCliInput,
  parseCliAction,
 )

main :: IO ()
main = do
  MkCliInput {cliVerbosity, cliActor} <- getCliInput

  let hydraVerbosity = if cliVerbosity then Verbose "hydra-auction" else Quiet
  tracer <- stdoutOrNullTracer hydraVerbosity

  -- FIXME: We need to pass in the cardano-node as a parameter to the CLI
  -- This way multiple CLI instances can share the same cardano node.
  -- At the moment, only Alice will start the node, other users will assume the
  -- node is present.
  when (cliActor == Alice) $ do
    void $ async $ runCardanoNode (contramap FromHydra tracer)

  putStrLn ("Starting CLI for " <> show cliActor)
  node <- getCardanoNode

  let runnerContext =
        MkExecutionContext
          { tracer = tracer
          , node = node
          , actor = cliActor
          }

  executeRunner runnerContext loopCLI

loopCLI :: Runner ()
loopCLI = do
  result <- liftIO $ try @SomeException getLine
  case result of
    Left ex -> do
      liftIO $ putStrLn $ "input error: " <> displayException ex
      pure ()
    Right command -> do
      case parseCliAction $ words command of
        Left e -> do
          liftIO $ putStrLn e
          loopCLI
        Right cmd -> do
          handleCliAction cmd
          loopCLI
