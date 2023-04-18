module HydraAuction.Hydra.Runner (
  HydraRunner (..),
  HydraExecutionContext (..),
  matchingHydraEvent,
  executeHydraRunnerFakingParams,
  executeHydraRunner,
) where

-- Prelude imports
import Prelude

-- Haskell imports

import Control.Lens ((^?))
import Control.Lens.Getter (Getting)
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
import Data.Monoid (First)
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
  EventMatcher (..),
  MonadHydra (..),
 )
import HydraAuction.Runner (Runner)
import HydraAuctionUtils.BundledData (readHydraNodeProtocolParams)
import HydraAuctionUtils.Monads (
  BlockchainParams (..),
  MonadBlockchainParams (..),
  MonadNetworkId (askNetworkId),
  MonadTrace (..),
 )

data HydraRunnerLog
  = HydraRunnerStringMessage String
  | AwaitingHydraEvent AwaitedHydraEvent
  | AwaitedHydraEvent HydraEvent
  deriving stock (Show)

data HydraExecutionContext = MkHydraExecutionContext
  { node :: HydraClient
  , tracer :: Tracer IO HydraRunnerLog
  , fakeBlockchainParams :: BlockchainParams
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
    traceMessage $ AwaitingHydraEvent awaitedSpec
    -- FIXME: raise custom errors
    event <- liftIO $ waitMatch timeout node matchingHandler
    traceMessage $ AwaitedHydraEvent event
    return event
    where
      matchingHandler value = do
        event <- matchingHydraEvent value
        guard $ matchingPredicate event
        return event
      matchingPredicate event = case awaitedSpec of
        Any -> True
        SpecificKind expectedKind -> getHydraEventKind event == expectedKind
        SpecificEvent expectedEvent -> event == expectedEvent
        CustomMatcher (EventMatcher customMatcher) -> customMatcher event

instance MonadNetworkId HydraRunner where
  askNetworkId = return $ Testnet $ NetworkMagic 42

instance MonadTrace HydraRunner where
  type TracerMessage HydraRunner = HydraRunnerLog
  stringToMessage = HydraRunnerStringMessage
  traceMessage message = do
    MkHydraExecutionContext {tracer} <- ask
    liftIO $ traceWith tracer message

instance MonadBlockchainParams HydraRunner where
  queryBlockchainParams = do
    MkHydraExecutionContext {fakeBlockchainParams} <- ask
    return fakeBlockchainParams

  -- Hydra slot is always 0, so it probably safe to use it
  -- for interval conversion
  toSlotNo _ = return 0

matchingHydraEvent :: Value -> Maybe HydraEvent
matchingHydraEvent value =
  case value ^? key "tag" of
    Just "GetUTxOResponse" -> getUtxoValueHandler
    Just "HeadIsInitializing" ->
      HeadIsInitializing <$> retrieveField "headId"
    Just "TxSeen" -> TxSeen <$> retrieveField "tx"
    Just "TxValid" -> TxValid <$> retrieveField "tx"
    Just "TxInvalid" -> TxInvalid <$> retrieveField "tx"
    Just "InvlidInput" ->
      InvlidInput <$> retrieveField "reason" <*> retrieveField "input"
    Just "PostTxOnChainFailed" ->
      PostTxOnChainFailed
        <$> retrieveField' (key "postChainTx" . key "tag")
        <*> retrieveField' (key "postTxError" . key "tag")
    Just "CommandFailed" ->
      CommandFailed
        <$> retrieveField'
          (key "clientInput" . key "tag")
    Just "SnapshotConfirmed" ->
      SnapshotConfirmed
        <$> retrieveField'
          ( key "snapshot" . key "confirmedTransactions"
          )
        <*> retrieveField'
          ( key "snapshot" . key "utxo"
          )
    Just "Committed" -> Committed <$> retrieveField "utxo"
    Just "HeadIsOpen" -> HeadIsOpen <$> retrieveField "utxo"
    Just "HeadIsClosed" -> Just HeadIsClosed
    Just "ReadyToFanout" -> Just ReadyToFanout
    Just "HeadIsFinalized" -> HeadIsFinalized <$> retrieveField "utxo"
    _ -> Nothing
  where
    getUtxoValueHandler =
      GetUTxOResponse <$> do
        txInOutMap <- retrieveField "utxo"
        return $ UTxO.fromPairs $ Map.toList txInOutMap
    retrieveField :: FromJSON a => Key -> Maybe a
    retrieveField fieldKey = retrieveField' $ key fieldKey
    retrieveField' ::
      FromJSON a => Getting (First Value) Value Value -> Maybe a
    retrieveField' getter = do
      fieldValue <- value ^? getter
      parseMaybe parseJSON fieldValue

executeHydraRunner ::
  HydraExecutionContext ->
  HydraRunner a ->
  IO a
executeHydraRunner context runner =
  runReaderT (unHydraRunner runner) context

executeHydraRunnerFakingParams :: HydraClient -> HydraRunner a -> Runner a
executeHydraRunnerFakingParams node monad = do
  params <- queryBlockchainParams
  protocolParameters <- liftIO readHydraNodeProtocolParams
  let patchedParams =
        params
          { protocolParameters = protocolParameters
          }
  liftIO $ executeHydraRunner (context patchedParams) monad
  where
    tracer = contramap show stdoutTracer
    context params =
      MkHydraExecutionContext
        { node = node
        , tracer = tracer
        , fakeBlockchainParams = params
        }
