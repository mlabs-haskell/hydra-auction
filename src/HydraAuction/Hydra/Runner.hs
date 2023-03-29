module HydraAuction.Hydra.Runner (
  HydraRunner (..),
  matchingHydraEvent,
  executeHydraRunnerInTest,
  executeHydraRunner,
) where

-- Prelude imports
import Prelude

-- Haskell imports

import Control.Lens ((^?))
import Control.Monad (guard)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), ask)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Tracer (Tracer, contramap, stdoutTracer, traceWith)
import Data.Aeson (
  FromJSON,
  Key,
  Value,
  parseJSON,
  (.=),
 )
import Data.Aeson.Lens (key)
import Data.Aeson.Types (parseMaybe)
import Data.Map qualified as Map
import GHC.Natural (Natural)

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports
import Cardano.Api (NetworkId (Testnet))
import Hydra.Cardano.Api (NetworkMagic (NetworkMagic))
import HydraNode (
  HydraClient,
  input,
  send,
  waitMatch,
 )

-- HydraAuction imports
import HydraAuction.Hydra.Interface (
  HydraCommand (..),
  HydraEvent (..),
  commandConstructorName,
  getHydraEventKind,
 )
import HydraAuction.Hydra.Monad (
  AwaitedHydraEvent (..),
  MonadHydra (..),
 )
import HydraAuctionUtils.Monads (
  MonadNetworkId (askNetworkId),
  MonadTrace (..),
 )

{- HLINT ignore "Use newtype instead of data" -}
data HydraRunnerLog = HydraRunnerStringMessage String
  deriving stock (Show)

data HydraExecutionContext = MkHydraExecutionContext
  { node :: HydraClient
  , tracer :: Tracer IO HydraRunnerLog
  }

newtype HydraRunner a = MkHydraRunner
  {unHydraRunner :: ReaderT HydraExecutionContext IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader HydraExecutionContext
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance MonadHydra HydraRunner where
  sendCommand :: HydraCommand -> HydraRunner ()
  sendCommand command = do
    MkHydraExecutionContext {node} <- ask
    liftIO $ send node $ input commandText commandArguments
    where
      commandText = commandConstructorName command
      commandArguments = case command of
        Commit utxo -> ["utxo" .= utxo]
        NewTx tx -> ["transaction" .= tx]
        _ -> []

  waitForHydraEvent' :: Natural -> AwaitedHydraEvent -> HydraRunner HydraEvent
  waitForHydraEvent' timeout awaitedSpec = do
    MkHydraExecutionContext {node} <- ask
    liftIO $ waitMatch timeout node matchingHandler
    where
      matchingHandler value = do
        event <- matchingHydraEvent value
        case awaitedSpec of
          Any -> return ()
          SpecificKind expectedKind -> guard $ getHydraEventKind event == expectedKind
          SpecificEvent expectedEvent -> guard $ event == expectedEvent
        return event

instance MonadNetworkId HydraRunner where
  askNetworkId = return $ Testnet $ NetworkMagic 42

instance MonadTrace HydraRunner where
  type TracerMessage HydraRunner = HydraRunnerLog
  stringToMessage = HydraRunnerStringMessage
  traceMessage message = do
    MkHydraExecutionContext {tracer} <- ask
    liftIO $ traceWith tracer message

matchingHydraEvent :: Value -> Maybe HydraEvent
matchingHydraEvent value =
  case value ^? key "tag" of
    Just "GetUTxOResponse" -> getUtxoValueHandler
    Just "HeadIsInitializing" ->
      HeadIsInitializing <$> retrieveField "headId"
    Just "TxSeen" -> TxSeen <$> retrieveField "tx"
    Just "Committed" -> Committed <$> retrieveField "utxo"
    Just "HeadIsOpen" -> Just HeadIsOpen
    Just "HeadIsClosed" -> Just HeadIsClosed
    Just "ReadyToFanout" -> Just ReadyToFanout
    Just "HeadIsFinalized" -> Just HeadIsFinalized
    _ -> Nothing
  where
    getUtxoValueHandler =
      GetUTxOResponse <$> do
        txInOutMap <- retrieveField "utxo"
        return $ UTxO.fromPairs $ Map.toList txInOutMap
    retrieveField :: FromJSON a => Key -> Maybe a
    retrieveField fieldKey = do
      fieldValue <- value ^? key fieldKey
      parseMaybe parseJSON fieldValue

executeHydraRunner ::
  HydraExecutionContext ->
  HydraRunner a ->
  IO a
executeHydraRunner context runner =
  runReaderT (unHydraRunner runner) context

executeHydraRunnerInTest :: HydraClient -> HydraRunner a -> IO a
executeHydraRunnerInTest node = executeHydraRunner context
  where
    tracer = contramap show stdoutTracer
    context = MkHydraExecutionContext {node = node, tracer = tracer}
