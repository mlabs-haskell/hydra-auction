{-# OPTIONS_GHC -w #-}

module Main (main) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Concurrent.Async (withAsync)
import Control.Monad (void, when)
import Control.Monad.Catch (try)
import Control.Monad.Trans.Class (lift)
import System.Console.Haskeline (
  InputT,
  defaultSettings,
  getInputLine,
  runInputT,
 )

-- Hydra imports
import Hydra.Logging (Verbosity (Quiet, Verbose))
import Hydra.Prelude (SomeException, ask, contramap, liftIO)

-- Hydra auction imports

import HydraAuction.Fixture (Actor (..))
import HydraAuction.Runner (
  ExecutionContext (..),
  HydraAuctionLog (FromHydra),
  Runner,
  executeRunner,
  stdoutOrNullTracer,
 )

import CardanoNode (
  RunningNode (
    RunningNode,
    networkId,
    nodeSocket
  ),
 )

-- Hydra auction CLI imports
import CLI.Actions (CliAction, handleCliAction)
import CLI.Parsers (
  CliInput (..),
  getCliInput,
  parseCliAction,
 )

main :: IO ()
main = do
  MkCliInput {cliVerbosity, cliActor, cliNodeSocket, cliNetworkId} <- getCliInput

  let hydraVerbosity = if cliVerbosity then Verbose "hydra-auction" else Quiet
  tracer <- stdoutOrNullTracer hydraVerbosity

  let node = RunningNode {nodeSocket = cliNodeSocket, networkId = cliNetworkId}

      runnerContext =
        MkExecutionContext
          { tracer = tracer
          , node = node
          , actor = cliActor
          }

  executeRunner runnerContext loopCLI

loopCLI :: Runner ()
loopCLI = do
  ctx <- ask
  let promtString = show (actor ctx) <> "> "
  let loop :: InputT Runner ()
      loop = do
        minput <- getInputLine promtString
        case minput of
          Nothing -> pure ()
          Just "quit" -> pure ()
          Just input -> do
            lift $ handleInput input
            liftIO $ putStrLn ""
            loop
  runInputT defaultSettings loop
  where
    handleInput :: String -> Runner ()
    handleInput input = case parseCliAction $ words input of
      Left e -> liftIO $ putStrLn e
      Right cmd -> handleCliAction' cmd
    handleCliAction' :: CliAction -> Runner ()
    handleCliAction' cmd = do
      result <- try $ handleCliAction cmd
      case result of
        Right _ -> pure ()
        Left (actionError :: SomeException) ->
          liftIO $
            putStrLn $
              "Error during CLI action handling: \n\n" <> show actionError
