module HydraAuctionUtils.Hydra.Runner (
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
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, try)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), ask)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Tracer (Tracer, contramap, stdoutTracer, traceWith)
import Data.Aeson (
  FromJSON,
  Key,
  Value,
  parseJSON,
  toJSON,
  (.=),
 )
import Data.Aeson.Lens (key)
import Data.Aeson.Types (parseMaybe)
import Data.Map qualified as Map
import Data.Monoid (First)
import GHC.Natural (Natural, naturalToInt)
import Test.HUnit.Lang (HUnitFailure)

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports
import Cardano.Api (NetworkId (Testnet))
import Hydra.Cardano.Api (
  NetworkMagic (NetworkMagic),
  pattern TxValidityNoLowerBound,
  pattern TxValidityNoUpperBound,
 )
import HydraNode (
  HydraClient,
  input,
  send,
  waitMatch,
 )

-- HydraAuction imports

import HydraAuctionUtils.BundledData (readHydraNodeProtocolParams)
import HydraAuctionUtils.Hydra.Interface (
  HydraCommand (..),
  HydraEvent (..),
  getHydraEventKind,
 )
import HydraAuctionUtils.Hydra.Monad (
  AwaitedHydraEvent (..),
  EventMatcher (..),
  MonadHydra (..),
 )
import HydraAuctionUtils.L1.Runner (L1Runner)
import HydraAuctionUtils.Monads (
  BlockchainParams (..),
  MonadBlockchainParams (..),
  MonadNetworkId (askNetworkId),
  MonadTrace (..),
 )

data HydraRunnerLog
  = HydraRunnerStringMessage String
  | SendCommand HydraCommand
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
    traceMessage $ SendCommand command
    liftIO $ send node $ toJSON command

  waitForHydraEvent' ::
    Natural -> AwaitedHydraEvent -> HydraRunner (Maybe HydraEvent)
  waitForHydraEvent' timeoutSeconds awaitedSpec = do
    MkHydraExecutionContext {node} <- ask
    traceMessage $ AwaitingHydraEvent awaitedSpec
    mEvent <- liftIO $ try $ waitMatch timeout node matchingHandler
    case mEvent of
      Left (_ :: HUnitFailure) -> return Nothing
      Right event -> do
        traceMessage $ AwaitedHydraEvent event
        return $ Just event
    where
      timeout = fromInteger $ toInteger $ naturalToInt timeoutSeconds
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

  -- FIXME: Hydra started to support time but we still do not
  convertValidityBound (_, _) =
    return (TxValidityNoLowerBound, TxValidityNoUpperBound)

matchingHydraEvent :: Value -> Maybe HydraEvent
matchingHydraEvent value =
  case value ^? key "tag" of
    Just "GetUTxOResponse" -> getUtxoValueHandler
    Just "HeadIsInitializing" ->
      HeadIsInitializing <$> retrieveField "headId"
    Just "TxSeen" -> TxSeen <$> retrieveField "transaction"
    Just "TxValid" -> TxValid <$> retrieveField "transaction"
    Just "TxInvalid" ->
      TxInvalid <$> retrieveField "transaction" <*> retrieveField "utxo"
    Just "InvalidInput" ->
      InvalidInput <$> retrieveField "reason" <*> retrieveField "input"
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
    Just "Committed" ->
      Committed <$> retrieveField "utxo" <*> retrieveField "party"
    Just "HeadIsOpen" -> HeadIsOpen <$> retrieveField "utxo"
    Just "HeadIsClosed" -> Just HeadIsClosed
    Just "ReadyToFanout" -> Just ReadyToFanout
    Just "HeadIsFinalized" -> HeadIsFinalized <$> retrieveField "utxo"
    Just "HeadIsAborted" -> Just HeadIsAborted
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

executeHydraRunnerFakingParams :: HydraClient -> HydraRunner a -> L1Runner a
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
