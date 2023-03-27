module HydraAuction.Hydra.Runner where

-- Prelude imports
import Prelude

-- Haskell imports

import Control.Lens ((^?))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), ask)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson (
  parseJSON,
  (.=),
  Value,
 )
import Data.Aeson.Lens (key)
import Data.Aeson.Types (parseMaybe)
import Data.Map qualified as Map
import GHC.Natural (Natural)

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports
import HydraNode (
  HydraClient,
  input,
  send,
  waitMatch,
 )
import Cardano.Api (NetworkId(Testnet))
import Hydra.Cardano.Api (NetworkMagic(NetworkMagic))

-- HydraAuction imports

import Control.Monad (guard)
import HydraAuction.Hydra.Interface (
  HydraCommand (..),
  HydraEvent (..),
  commandConstructorName,
  getHydraEventKind,
 )
import HydraAuction.Hydra.Monad (AwaitedHydraEvent (..), MonadHydra (..))
import HydraAuctionUtils.Monads (MonadNetworkId (askNetworkId), MonadTrace (..))

data HydraExecutionContext = MkHydraExecutionContext {node :: !HydraClient}

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
  type TracerMessage HydraRunner = ()
  stringToMessage = undefined
  traceMessage _message = return ()

matchingHydraEvent :: Value -> Maybe HydraEvent
matchingHydraEvent value =
  case value ^? key "tag" of
    Just "GetUTxOResponse" -> getUtxoValueHandler value
    Just "HeadIsInitializing" ->
      HeadIsInitializing <$> retrieveField "headId" value
    Just "TxSeen" -> TxSeen <$> retrieveField "tx" value
    Just "Committed" -> Committed <$> retrieveField "utxo" value
    Just "HeadIsOpen" -> Just HeadIsOpen
    Just "HeadIsClosed" -> Just HeadIsClosed
    Just "ReadyToFanout" -> Just ReadyToFanout
    Just "HeadIsFinalized" -> Just HeadIsFinalized
    _ -> Nothing
  where
    getUtxoValueHandler value =
      GetUTxOResponse <$> do
        txInOutMap <- retrieveField "utxo" value
        return $ UTxO.fromPairs $ Map.toList txInOutMap
    retrieveField fieldKey value = do
      fieldValue <- value ^? key fieldKey
      parseMaybe parseJSON fieldValue

executeRunner ::
  HydraExecutionContext ->
  HydraRunner a ->
  IO a
executeRunner context runner =
  runReaderT (unHydraRunner runner) context

executeRunnerInTest :: HydraClient -> HydraRunner a -> IO a
executeRunnerInTest node runner = executeRunner context runner
  where
    context = MkHydraExecutionContext {node = node}
