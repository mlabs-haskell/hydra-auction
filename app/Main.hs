{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Concurrent.Async (withAsync)
import Control.Tracer (stdoutTracer, traceWith)
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
import Hydra.Network (Host (..))
import Hydra.Prelude (SomeException)

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
  L1Runner,
  executeL1RunnerWithNodeAs,
 )
import HydraAuctionUtils.Monads.Actors (MonadHasActor (..), WithActorT)

-- Hydra auction CLI imports
import CLI.Actions (CliAction, handleCliAction)
import CLI.Parsers (
  CliInput,
  CliOptions (InteractivePrompt, Watch),
  PromptOptions (..),
  cliOptions,
  delegateSettings,
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

handleCliInput :: CliInput -> IO ()
handleCliInput input = do
  -- Connect to delegate server
  -- Await for initialized DelegateState
  -- FIXME: refactor this out

  let settings = delegateSettings input
  currentDelegateStateRef <- newIORef initialState

  result <- trySome $ runClient (Text.unpack $ hostname settings) (fromIntegral $ port settings) "/" $ \client -> do
    let tracer = contramap (show . pretty) stdoutTracer
    sendTextData client . encode $ QueryCurrentDelegateState
    -- DelegateState receiving thread
    withAsync (updateStateThread client currentDelegateStateRef tracer) $ \_ -> do
      handleCliInput' (Just client) currentDelegateStateRef (cliOptions input)

  case result of
    Right x -> return x
    Left _ -> handleCliInput' Nothing currentDelegateStateRef (cliOptions input)
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

handleCliInput' :: Maybe Connection -> IORef DelegateState -> CliOptions -> IO ()
handleCliInput' mClient currentDelegateStateRef input = case input of
  (Watch auctionName) -> watchAuction auctionName currentDelegateStateRef
  (InteractivePrompt MkPromptOptions {..}) ->
    executeL1RunnerWithNodeAs
      cliCardanoNode
      cliActor
      (action client currentDelegateStateRef)
    where
      client = case mClient of
        Just x -> x
        -- FIXME
        Nothing -> error "Delegate server is not started yet"
      action =
        case cliNoninteractiveAction of
          Just actionStr -> \y z -> handleREPLInput y z actionStr
          Nothing -> loopCLI

loopCLI :: Connection -> IORef DelegateState -> WithActorT L1Runner ()
loopCLI client currentDelegateStateRef = do
  actor <- askActor
  let promptString = show actor <> "> "
      handler = handleREPLInput client currentDelegateStateRef
  runInputT defaultSettings $ genericREPLLoop promptString handler

handleREPLInput :: Connection -> IORef DelegateState -> String -> WithActorT L1Runner ()
handleREPLInput client currentDelegateStateRef input =
  case parseCliAction $ words input of
    Left e -> liftIO $ putStrLn e
    Right cmd -> handleCliAction' cmd
  where
    sendRequestToDelegate = sendTextData client . encode
    handleCliAction' :: CliAction -> WithActorT L1Runner ()
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

genericREPLLoop ::
  (MonadMask m, MonadIO m) => String -> (String -> m ()) -> InputT m ()
genericREPLLoop prompt handler = do
  minput <- getInputLine prompt
  case minput of
    Nothing -> pure ()
    Just "quit" -> pure ()
    Just input -> do
      lift $ handler input
      liftIO $ putStrLn ""
      genericREPLLoop prompt handler
