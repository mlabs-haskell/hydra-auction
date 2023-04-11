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
  CliInput (InteractivePrompt, Watch),
  PromptOptions (..),
  getCliInput,
  parseCliAction,
 )
import CLI.Watch (
  watchAuction,
 )
import Control.Concurrent.Async (withAsync)
import Control.Monad (forever)
import Data.Aeson (decode, encode)
import Data.IORef (IORef, newIORef, writeIORef)
import Data.Maybe (fromJust)
import HydraAuction.Delegate.Interface (DelegateResponse (..), DelegateState, FrontendRequest (QueryCurrentDelegateState), initialState)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  ci <- getCliInput
  handleCliInput ci

handleCliInput :: CliInput -> IO ()
handleCliInput input = do
  -- Connect to delegate server
  -- TODO: port change
  -- FIXUP: parse actual adress and other params
  delegateNumber <- read . fromJust <$> lookupEnv "DELEGATE_NUMBER"
  runClient "127.0.0.1" (8000 + delegateNumber) "/" $ \client -> do
    currentDelegateStateRef <- newIORef initialState
    sendTextData client . encode $ QueryCurrentDelegateState
    -- DelegateState receiving thread
    withAsync (updateStateThread client currentDelegateStateRef) $ \_ -> do
      handleCliInput' client currentDelegateStateRef input
  where
    updateStateThread client currentDelegateStateRef = forever $ do
      mMessage <- decode <$> receiveData client
      case mMessage of
        Just (CurrentDelegateState _ state) ->
          writeIORef currentDelegateStateRef state
        _ -> putStrLn $ "Ignoring server message: " <> show mMessage

handleCliInput' :: _ -> IORef _ -> CliInput -> IO ()
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
