{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- Prelude imports
import Prelude

-- Haskell imports

import Control.Concurrent.Async (withAsync)
import Control.Monad (forever)
import Control.Monad.Catch (try)
import Control.Monad.Trans.Class (lift)
import Control.Tracer (contramap, stdoutTracer, traceWith)
import Data.Aeson (eitherDecode, encode)
import Data.IORef (IORef, newIORef, writeIORef)
import Data.Text qualified as Text
import Network.WebSockets (
  Connection,
  receiveData,
  runClient,
  sendTextData,
 )
import Prettyprinter (Pretty (pretty))
import System.Console.Haskeline (
  InputT,
  defaultSettings,
  getInputLine,
  runInputT,
 )

-- Hydra imports
import Hydra.Logging (Verbosity (Quiet, Verbose))
import Hydra.Network (Host (..))
import Hydra.Prelude (SomeException, ask, liftIO)

-- Hydra auction imports

import HydraAuction.Delegate.Interface (
  DelegateResponse (..),
  DelegateState,
  FrontendRequest (QueryCurrentDelegateState),
  IncorrectRequestDataReason (..),
  RequestIgnoredReason (..),
  initialState,
 )
import HydraAuctionUtils.L1.Runner (
  ExecutionContext (..),
  L1Runner,
  executeL1Runner,
  stdoutOrNullTracer,
 )

-- Hydra auction CLI imports
import CLI.Actions (CliAction, handleCliAction)
import CLI.Parsers (
  CliInput (..),
  CliOptions (InteractivePrompt, Watch),
  PromptOptions (..),
  getCliInput,
  parseCliAction,
 )
import CLI.Types (CLIError (..))
import CLI.Watch (
  watchAuction,
 )

main :: IO ()
main = do
  ci <- getCliInput
  handleCliInput ci

runClientFromHost :: Host -> (Connection -> IO ()) -> IO ()
runClientFromHost settings = runClient (Text.unpack $ hostname settings) (fromIntegral $ port settings) "/"

handleCliInput :: CliInput -> IO ()
handleCliInput input = do
  -- Connect to delegate server
  runClientFromHost (delegateSettings input) $ \client -> do
    currentDelegateStateRef <- newIORef initialState
    let tracer = contramap (show . pretty) stdoutTracer
    sendTextData client . encode $ QueryCurrentDelegateState
    -- DelegateState receiving thread
    withAsync (updateStateThread client currentDelegateStateRef tracer) $ \_ -> do
      handleCliInput' client currentDelegateStateRef input
  where
    updateStateThread client currentDelegateStateRef tracer = forever $ do
      mMessage <- eitherDecode <$> receiveData client
      -- FIXME: separate logic and messages
      -- FIXME: concurrent stdout to REPL
      case mMessage of
        Right response ->
          case response of
            (CurrentDelegateState _ state) ->
              writeIORef currentDelegateStateRef state
            RequestIgnored reason -> case reason of
              (IncorrectRequestData AuctionTermsAreInvalidOrNotMatchingHead) -> do
                putStrLn "Delegate server says Auction Terms are incorrect."
                putStrLn "Probably you are connected to wrong Hydra Head."
              (IncorrectRequestData InvalidBidTerms) -> notExpectedResponse
              (IncorrectRequestData TxIdDoesNotExist) -> notExpectedResponse
              (WrongDelegateState _) -> notExpectedResponse
            ClosingTxTemplate -> return ()
            AuctionSet {} -> return ()
          where
            notExpectedResponse = do
              putStrLn "Not expected response from delegate server."
              putStrLn "That probably means Delegate state sync problem."
              putStrLn "Or that is a bug in Frontend CLI."
        Left err -> traceWith tracer (InvalidDelegateResponse err)

handleCliInput' :: Connection -> IORef DelegateState -> CliInput -> IO ()
handleCliInput' delegateConnection currentDelegateStateRef input = case cliOptions input of
  (Watch auctionName) -> watchAuction auctionName currentDelegateStateRef
  (InteractivePrompt MkPromptOptions {..}) -> do
    -- Connect to platform server
    runClientFromHost (platformSettings input) $ \platformConnection -> do
      let hydraVerbosity = if cliVerbosity then Verbose "hydra-auction" else Quiet
      tracer <- stdoutOrNullTracer hydraVerbosity

      let runnerContext =
            MkExecutionContext
              { tracer = tracer
              , node = cliCardanoNode
              , actor = cliActor
              }

      executeL1Runner runnerContext (loopCLI delegateConnection platformConnection currentDelegateStateRef)

loopCLI :: Connection -> Connection -> IORef DelegateState -> L1Runner ()
loopCLI delegateConnection platformConnection currentDelegateStateRef = do
  ctx <- ask
  let promptString = show (actor ctx) <> "> "

      loop :: InputT L1Runner ()
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
    sendRequestToDelegate = sendTextData delegateConnection . encode
    sendRequestToPlatform input = do
      sendTextData platformConnection $ encode input
      mMessage <- eitherDecode <$> receiveData platformConnection
      case mMessage of
        Right m -> pure m
        Left e -> error $ "Error decoding platform output: \n\n" <> show e

    handleInput :: String -> L1Runner ()
    handleInput input = case parseCliAction $ words input of
      Left e -> liftIO $ putStrLn e
      Right cmd -> handleCliAction' cmd
    handleCliAction' :: CliAction -> L1Runner ()
    handleCliAction' cmd = do
      result <-
        try $
          handleCliAction sendRequestToDelegate sendRequestToPlatform currentDelegateStateRef cmd
      case result of
        Right _ -> pure ()
        Left (actionError :: SomeException) ->
          liftIO $
            putStrLn $
              "Error during CLI action handling: \n\n" <> show actionError
