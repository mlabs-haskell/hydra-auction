module Main (main) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (void, when)

-- Hydra imports

import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Logging (Verbosity (Quiet, Verbose))
import Hydra.Prelude (contramap, liftIO)

-- Hydra auction imports
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
  CliInput (MkCliInput, ciActor, ciVerbosity),
  getCliInput,
  parseCliAction,
 )

main :: IO ()
main = do
  MkCliInput {ciVerbosity, ciActor} <- getCliInput

  let hydraVerbosity = if ciVerbosity then Verbose "hydra-auction" else Quiet
  tracer <- stdoutOrNullTracer hydraVerbosity
  when (ciActor == Alice) $ do
    void $ async $ runCardanoNode (contramap FromHydra tracer)
  putStrLn ("Starting CLI for " <> show ciActor)
  node <- getCardanoNode

  let runnerContext =
        MkExecutionContext
          { tracer = tracer
          , node = node
          , actor = ciActor
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
