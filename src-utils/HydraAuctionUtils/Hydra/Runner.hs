module HydraAuctionUtils.Hydra.Runner (
  HydraRunner (..),
  HydraExecutionContext (..),
  executeHydraRunnerFakingParams,
  executeHydraRunner,
  runL1RunnerInComposite,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Tracer (Tracer, stdoutTracer, traceWith)
import GHC.Natural (Natural, naturalToInt)
import System.Timeout (timeout)

-- HydraAuction imports

import HydraAuctionUtils.BundledData (readHydraNodeProtocolParams)
import HydraAuctionUtils.Fixture (Actor)
import HydraAuctionUtils.Hydra.Interface (
  HydraCommand,
  HydraEvent,
  HydraProtocol,
 )
import HydraAuctionUtils.Hydra.Monad (
  AwaitedHydraEvent,
  MonadHydra (..),
  ViaMonadHydra (..),
 )
import HydraAuctionUtils.L1.Runner (
  ExecutionContext,
  L1Runner,
  executeL1Runner,
 )
import HydraAuctionUtils.Monads (
  BlockchainParams (..),
  MonadBlockchainParams (..),
  MonadNetworkId (askNetworkId),
  MonadQueryUtxo (..),
  MonadSubmitTx (..),
  MonadTrace (..),
 )
import HydraAuctionUtils.Monads.Actors (
  MonadHasActor (..),
  WithActorT,
  withActor,
 )
import HydraAuctionUtils.Server.Client (
  AwaitedOutput,
  RealProtocolClient,
  waitForMatchingOutputH,
 )
import HydraAuctionUtils.Server.Protocol (ProtocolClient (..))

data HydraRunnerLog
  = HydraRunnerStringMessage String
  | SendCommand HydraCommand
  | AwaitingHydraEvent AwaitedHydraEvent
  | AwaitedHydraEvent HydraEvent
  deriving stock (Show)

data HydraExecutionContext = MkHydraExecutionContext
  { node :: RealProtocolClient HydraProtocol
  , tracer :: Tracer IO HydraRunnerLog
  , l1Context :: ExecutionContext
  , actor :: Actor
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
    , MonadBase IO
    , MonadBaseControl IO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )
  deriving (MonadSubmitTx, MonadQueryUtxo) via (ViaMonadHydra HydraRunner)

instance MonadHydra HydraRunner where
  sendCommand :: HydraCommand -> HydraRunner ()
  sendCommand command = do
    MkHydraExecutionContext {node} <- ask
    traceMessage $ SendCommand command
    liftIO $ sendInputH node command

  waitForHydraEvent' ::
    Natural -> AwaitedOutput HydraProtocol -> HydraRunner (Maybe HydraEvent)
  waitForHydraEvent' timeoutSeconds awaitedSpec = do
    MkHydraExecutionContext {node} <- ask
    traceMessage $ AwaitingHydraEvent awaitedSpec
    mResult <-
      liftIO $
        timeout timeoutMicroseconds $
          waitForMatchingOutputH node awaitedSpec
    case mResult of
      Just result -> traceMessage $ AwaitedHydraEvent result
      Nothing -> return ()
    return mResult
    where
      timeoutMicroseconds =
        1_000_000 * fromInteger (toInteger $ naturalToInt timeoutSeconds)

  runL1RunnerInComposite action = do
    actor <- askActor
    MkHydraExecutionContext {l1Context} <- ask
    liftIO $ executeL1Runner l1Context $ withActor actor action

instance MonadNetworkId HydraRunner where
  askNetworkId = runL1RunnerInComposite askNetworkId

instance MonadTrace HydraRunner where
  type TracerMessage HydraRunner = HydraRunnerLog
  stringToMessage = HydraRunnerStringMessage
  traceMessage message = do
    MkHydraExecutionContext {tracer} <- ask
    liftIO $ traceWith tracer message

instance MonadBlockchainParams HydraRunner where
  queryBlockchainParams = do
    params <- runL1RunnerInComposite queryBlockchainParams
    protocolParameters <- liftIO readHydraNodeProtocolParams
    return $ params {protocolParameters}

  queryCurrentSlot = runL1RunnerInComposite queryCurrentSlot
  convertValidityBound = runL1RunnerInComposite . convertValidityBound

  {-
  \| This records stats to underlying L1 Runner
  Not sure if this works well, but seems to match current L2 interation test
  -}
  recordTxStat = runL1RunnerInComposite . recordTxStat

instance MonadHasActor HydraRunner where
  askActor = actor <$> ask

executeHydraRunner ::
  HydraExecutionContext ->
  HydraRunner a ->
  IO a
executeHydraRunner context runner =
  runReaderT (unHydraRunner runner) context

executeHydraRunnerFakingParams ::
  forall a.
  RealProtocolClient HydraProtocol ->
  HydraRunner a ->
  WithActorT L1Runner a
executeHydraRunnerFakingParams node monad = do
  l1Context <- lift ask
  actor <- askActor
  liftIO $ executeHydraRunner (context l1Context actor) monad
  where
    tracer = contramap show stdoutTracer
    context l1Context actor =
      MkHydraExecutionContext
        { node = node
        , tracer = tracer
        , l1Context = l1Context
        , actor = actor
        }
