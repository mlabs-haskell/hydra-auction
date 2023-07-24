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
  CustomEvent (..),
  DelegateProtocol,
  DelegateResponse (..),
  DelegateState,
  FrontendRequest (QueryCurrentDelegateState),
  IncorrectRequestDataReason (..),
  RequestIgnoredReason (..),
  initialState,
 )
import HydraAuction.Platform.Interface (PlatformProtocol)
import HydraAuctionUtils.L1.Runner (
  L1Runner,
  executeL1RunnerWithNode,
  withActor,
 )
import HydraAuctionUtils.Monads.Actors (MonadHasActor (..), WithActorT)
import HydraAuctionUtils.WebSockets.Client (ProtocolClientFor, withProtocolClient)

-- Hydra auction CLI imports
import CLI.Actions (CliAction, CliActionHandle (..), handleCliAction)
import CLI.Parsers (
  CliInput (..),
  CliOptions (..),
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

handleCliInput :: CliInput -> IO ()
handleCliInput input = do
  -- Connect to delegate server
  -- Await for initialized DelegateState
  -- FIXME: refactor this out

  let settings = delegateSettings input
  currentDelegateStateRef <- newIORef initialState

  result <- trySome $ runClient (Text.unpack $ hostname settings) (fromIntegral $ port settings) "/" $ \client -> do
    let tracer = contramap (show . pretty) stdoutTracer
    sendTextData client . encode $ (QueryCurrentDelegateState @DelegateProtocol)
    -- DelegateState receiving thread
    withAsync (updateStateThread client currentDelegateStateRef tracer) $ \_ -> do
      handleCliInput' (Just client) currentDelegateStateRef input

  case result of
    Right x -> return x
    Left _ -> handleCliInput' Nothing currentDelegateStateRef input
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
            CustomEventHappened customEvent -> case customEvent of
              AuctionStageStarted _ stage -> do
                putStrLn $ "Auction stage started: " <> show stage
                return ()
              AuctionSet _ -> return ()
            TxEvent {} -> return ()
            ServerError message -> do
              putStrLn $ "Server error: " <> message
              return ()
          where
            notExpectedResponse = do
              putStrLn "Not expected response from delegate server."
              putStrLn "That probably means Delegate state sync problem."
              putStrLn "Or that is a bug in Frontend CLI."
        Left err -> traceWith tracer (InvalidDelegateResponse err)

handleCliInput' ::
  Maybe Connection -> IORef (DelegateState DelegateProtocol) -> CliInput -> IO ()
handleCliInput' mClient currentDelegateStateRef input =
  executeL1RunnerWithNode (cliCardanoNode input) $ case cliOptions input of
    (Watch auctionName) ->
      watchAuction auctionName currentDelegateStateRef
    (InteractivePrompt options) ->
      withActor (cliActor options) $ withHandle $ action options
  where
    withHandle cont = case mClient of
      Just client ->
        withProtocolClient (Host "127.0.0.1" 8010) () $
          \platformClient ->
            cont $
              MkCliActionHandle
                { platformClient
                , sendRequestToDelegate = sendTextData client . encode
                , currentDelegateStateRef
                }
      -- FIXME
      Nothing ->
        cont (error "Delegate server is not started yet")
    action options =
      case cliNoninteractiveAction options of
        Just actionStr -> flip handleREPLInput actionStr
        Nothing -> loopCLI

loopCLI ::
  forall client.
  ProtocolClientFor PlatformProtocol client =>
  CliActionHandle client ->
  WithActorT L1Runner ()
loopCLI handle = do
  actor <- askActor
  let promptString = show actor <> "> "
  runInputT defaultSettings $
    genericREPLLoop promptString $
      handleREPLInput handle

handleREPLInput ::
  forall client.
  ProtocolClientFor PlatformProtocol client =>
  CliActionHandle client ->
  String ->
  WithActorT L1Runner ()
handleREPLInput handle input =
  case parseCliAction $ words input of
    Left e -> liftIO $ putStrLn e
    Right cmd -> handleCliAction' cmd
  where
    handleCliAction' :: CliAction -> WithActorT L1Runner ()
    handleCliAction' cmd = do
      result <- try $ handleCliAction handle cmd
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
