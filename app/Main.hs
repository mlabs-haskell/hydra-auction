{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad.Catch (try)
import Control.Monad.Trans.Class (lift)
import Network.WebSockets (
  Connection,
  receiveData,
  runClient,
  sendTextData,
 )
import System.Console.Haskeline (
  InputT,
  defaultSettings,
  getInputLine,
  runInputT,
 )

-- Hydra imports
import Hydra.Logging (Verbosity (Quiet, Verbose))
import Hydra.Prelude (SomeException, ask, liftIO)

-- Hydra auction imports
import HydraAuction.Runner (
  ExecutionContext (..),
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
  CliInput,
  CliOptions (InteractivePrompt, Watch),
  PromptOptions (..),
  cliOptions,
  delegateIP,
  delegatePort,
  delegateSettings,
  getCliInput,
  parseCliAction,
 )
import CLI.Types (CLIError (InvalidResponse, UnimplementedResponse))
import CLI.Watch (
  watchAuction,
 )
import Control.Concurrent.Async (withAsync)
import Control.Monad (forever)
import Control.Tracer (contramap, stdoutTracer, traceWith)
import Data.Aeson (eitherDecode, encode)
import Data.IORef (IORef, newIORef, writeIORef)
import HydraAuction.Delegate.Interface (DelegateResponse (..), DelegateState, FrontendRequest (QueryCurrentDelegateState), initialState)
import Prettyprinter (Pretty (pretty))

main :: IO ()
main = do
  ci <- getCliInput
  handleCliInput ci

handleCliInput :: CliInput -> IO ()
handleCliInput input = do
  -- Connect to delegate server
  -- TODO: port change
  -- FIXUP: parse actual adress and other params
  let settings = delegateSettings input

  runClient (show $ delegateIP settings) (fromIntegral $ delegatePort settings) "/" $ \client -> do
    currentDelegateStateRef <- newIORef initialState
    let tracer = contramap (show . pretty) stdoutTracer
    sendTextData client . encode $ QueryCurrentDelegateState
    -- DelegateState receiving thread
    withAsync (updateStateThread client currentDelegateStateRef tracer) $ \_ -> do
      handleCliInput' client currentDelegateStateRef (cliOptions input)
  where
    updateStateThread client currentDelegateStateRef tracer = forever $ do
      mMessage <- eitherDecode <$> receiveData client
      case mMessage of
        Right (CurrentDelegateState _ state) ->
          writeIORef currentDelegateStateRef state
        Right response -> traceWith tracer (UnimplementedResponse response)
        Left err -> traceWith tracer (InvalidResponse err)

handleCliInput' :: Connection -> IORef DelegateState -> CliOptions -> IO ()
handleCliInput' client currentDelegateStateRef input = case input of
  (Watch auctionName) -> watchAuction auctionName currentDelegateStateRef
  (InteractivePrompt MkPromptOptions {..}) -> do
    let hydraVerbosity = if cliVerbosity then Verbose "hydra-auction" else Quiet
    tracer <- stdoutOrNullTracer hydraVerbosity

    let node = RunningNode {nodeSocket = cliNodeSocket, networkId = cliNetworkId}

        runnerContext =
          MkExecutionContext
            { tracer = tracer
            , node = node
            , actor = cliActor
            }

    executeRunner runnerContext (loopCLI client currentDelegateStateRef)

loopCLI :: Connection -> IORef DelegateState -> Runner ()
loopCLI client currentDelegateStateRef = do
  ctx <- ask
  let promptString = show (actor ctx) <> "> "

      loop :: InputT Runner ()
      loop = do
        minput <- getInputLine promptString
        case minput of
          Nothing -> pure ()
          Just "quit" -> pure ()
          Just input -> do
            lift $ handleInput input
            liftIO $ putStrLn ""
            loop
  runInputT defaultSettings loop
  where
    sendRequestToDelegate = sendTextData client . encode
    handleInput :: String -> Runner ()
    handleInput input = case parseCliAction $ words input of
      Left e -> liftIO $ putStrLn e
      Right cmd -> handleCliAction' cmd
    handleCliAction' :: CliAction -> Runner ()
    handleCliAction' cmd = do
      result <-
        try $
          handleCliAction sendRequestToDelegate currentDelegateStateRef cmd
      case result of
        Right _ -> pure ()
        Left (actionError :: SomeException) ->
          liftIO $
            putStrLn $
              "Error during CLI action handling: \n\n" <> show actionError
