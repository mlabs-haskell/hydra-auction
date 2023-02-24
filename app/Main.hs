module Main (main) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Concurrent.Async (withAsync)
import Control.Monad (void, when)
import Control.Monad.Catch (try)
import Control.Monad.Trans.Class (lift)
import System.Console.Haskeline

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
  let cardanoNodeRunner =
        when (cliActor == Alice) $ do
          putStrLn "Running cardano-node in background"
          void $ runCardanoNode (contramap FromHydra tracer)

  withAsync cardanoNodeRunner $ \_ -> do
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
    handleCliAction :: CliAction -> Runner ()
    handleCliAction' cmd = do
      result <- try $ handleCliAction cmd
      case result of
        Right _ -> pure ()
        Left (actionError :: SomeException) ->
          liftIO $
            putStrLn $
              "Error during CLI action handling: \n\n" <> show actionError
