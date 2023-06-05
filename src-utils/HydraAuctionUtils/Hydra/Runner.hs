module HydraAuctionUtils.Hydra.Runner (
  HydraRunner (..),
  HydraExecutionContext (..),
  executeHydraRunnerFakingParams,
  executeHydraRunner,
) where

-- Prelude imports
import Prelude

-- Haskell imports

import Control.Monad (guard)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, try)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), ask)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Tracer (Tracer, contramap, stdoutTracer, traceWith)
import Data.Aeson (
  Result (..),
  fromJSON,
  toJSON,
 )
import GHC.Natural (Natural, naturalToInt)
import Test.HUnit.Lang (HUnitFailure)

-- Hydra imports
import Cardano.Api (NetworkId (Testnet))
import Hydra.Cardano.Api (
  NetworkMagic (NetworkMagic),
  pattern TxValidityNoLowerBound,
  pattern TxValidityNoUpperBound,
 )
import Hydra.Chain.Direct.State ()
import HydraNode (
  HydraClient,
  send,
  waitMatch,
 )

-- HydraAuction imports

import HydraAuctionUtils.BundledData (readHydraNodeProtocolParams)
import HydraAuctionUtils.Hydra.Interface (
  HydraCommand,
  HydraEvent,
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
        event <- case fromJSON value of
          Success x -> Just x
          Error _ -> Nothing
        guard $ matchingPredicate event
        return event
      matchingPredicate event = case awaitedSpec of
        Any -> True
        SpecificKind expectedKind ->
          getHydraEventKind event == expectedKind
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
  convertValidityBound _ =
    return (TxValidityNoLowerBound, TxValidityNoUpperBound)

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
    tracer = contramap (const "TODO") stdoutTracer
    context params =
      MkHydraExecutionContext
        { node = node
        , tracer = tracer
        , fakeBlockchainParams = params
        }
