module HydraAuctionUtils.Hydra.Monad (
  ViaMonadHydra (..),
  MonadHydra (..),
  AwaitedHydraEvent,
  waitForHydraEvent,
  sendCommandAndWaitFor,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports

import GHC.Natural (Natural)

-- Cardano imports
import Cardano.Api (AddressInEra (..))
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports

import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (Address (ByronAddress, ShelleyAddress), Tx, pattern TxOut)
import Hydra.Snapshot (Snapshot (..))

-- HydraAuction imports
import HydraAuctionUtils.Hydra.Interface (
  HydraCommand,
  HydraEvent,
  HydraEventKind (GetUTxOResponseKind),
  HydraProtocol,
 )
import HydraAuctionUtils.L1.Runner (L1Runner)
import HydraAuctionUtils.Monads (
  MonadQueryUtxo (..),
  MonadSubmitTx (..),
  UtxoQuery (..),
 )
import HydraAuctionUtils.Monads.Actors (WithActorT)
import HydraAuctionUtils.Server.Client (AwaitedOutput (..), OutputMatcher (..))
import HydraAuctionUtils.Server.Protocol (WithClientT)

-- FIXME: this is for transition. Should be removed, maybe with ModadHydra
type AwaitedHydraEvent = AwaitedOutput HydraProtocol

class Monad m => MonadHydra m where
  sendCommand :: HydraCommand -> m ()
  waitForHydraEvent' :: Natural -> AwaitedHydraEvent -> m (Maybe HydraEvent)

  -- FIXME: rename and use polymorphic L1
  runL1RunnerInComposite :: forall a. WithActorT L1Runner a -> m a

defaultTimeout :: Natural
defaultTimeout = 30

waitForHydraEvent :: MonadHydra m => AwaitedHydraEvent -> m (Maybe HydraEvent)
waitForHydraEvent = waitForHydraEvent' defaultTimeout

sendCommandAndWaitFor ::
  MonadHydra m => AwaitedHydraEvent -> HydraCommand -> m (Maybe HydraEvent)
sendCommandAndWaitFor awaitedSpec command = do
  sendCommand command
  waitForHydraEvent awaitedSpec

newtype ViaMonadHydra m a = MkViaMonadHydra (m a)
  deriving newtype (Functor, Applicative, Monad, MonadHydra)

-- FIXME: this does not work without `Monad m`, do not know why
instance (Monad m, MonadHydra m) => MonadSubmitTx (ViaMonadHydra m) where
  submitTx tx = do
    sendCommand $ NewTx tx
    return $ Right ()

  awaitTx :: Tx -> ViaMonadHydra m ()
  awaitTx tx = do
    void $ waitForHydraEvent . CustomMatcher . OutputMatcher $ \case
      SnapshotConfirmed {snapshot} -> tx `elem` confirmed snapshot
      _ -> False

instance (Monad m, MonadHydra m) => MonadQueryUtxo (ViaMonadHydra m) where
  queryUtxo query = do
    response <-
      sendCommandAndWaitFor (SpecificKind GetUTxOResponseKind) GetUTxO
    let utxo' = case response of
          Just (GetUTxOResponse {utxo}) -> utxo
          Just _ -> error "Impossible happened: incorrect response type awaited"
          Nothing -> error "Hydra server not answering"
    return $ UTxO.fromPairs $ filter predicate $ UTxO.pairs utxo'
    where
      predicate (txIn, TxOut (AddressInEra _ txOutAddress) _ _ _) = case query of
        ByAddress address ->
          case txOutAddress of
            addr@ShelleyAddress {} ->
              address == addr
            ByronAddress _ -> error "didn't expect Byron address"
        ByTxIns txIns -> txIn `elem` txIns

instance MonadHydra m => MonadHydra (StateT s m) where
  sendCommand = lift . sendCommand
  waitForHydraEvent' timeout = lift . waitForHydraEvent' timeout
  runL1RunnerInComposite = lift . runL1RunnerInComposite

instance MonadHydra m => MonadHydra (WithClientT client m) where
  sendCommand = lift . sendCommand
  waitForHydraEvent' x = lift . waitForHydraEvent' x
  runL1RunnerInComposite = lift . runL1RunnerInComposite
