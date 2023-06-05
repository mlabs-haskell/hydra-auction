{-# OPTIONS_GHC -Wno-orphans #-}

module HydraAuctionUtils.Hydra.Monad (
  MonadHydra (..),
  EventMatcher (..),
  AwaitedHydraEvent (..),
  waitForHydraEvent,
  sendCommandAndWaitFor,
) where

-- Prelude imports
import Prelude

-- Haskell imports

import Control.Monad (void)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans (lift))
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
 )
import HydraAuctionUtils.Monads (
  MonadQueryUtxo (..),
  MonadSubmitTx (..),
  UtxoQuery (..),
 )

data AwaitedHydraEvent
  = Any
  | SpecificEvent HydraEvent
  | SpecificKind HydraEventKind
  | CustomMatcher EventMatcher
  deriving stock (Show)

newtype EventMatcher = EventMatcher (HydraEvent -> Bool)

instance Show EventMatcher where
  show (EventMatcher _) = "EventMatcher <some HydraEvent predicate>"

class Monad m => MonadHydra m where
  sendCommand :: HydraCommand -> m ()
  waitForHydraEvent' :: Natural -> AwaitedHydraEvent -> m (Maybe HydraEvent)

defaultTimeout :: Natural
defaultTimeout = 30

waitForHydraEvent :: MonadHydra m => AwaitedHydraEvent -> m (Maybe HydraEvent)
waitForHydraEvent = waitForHydraEvent' defaultTimeout

sendCommandAndWaitFor ::
  MonadHydra m => AwaitedHydraEvent -> HydraCommand -> m (Maybe HydraEvent)
sendCommandAndWaitFor awaitedSpec command = do
  sendCommand command
  waitForHydraEvent awaitedSpec

-- FIXME: this does not work without `Monad m`, do not know why
instance {-# OVERLAPPABLE #-} (Monad m, MonadHydra m) => MonadSubmitTx m where
  submitTx :: Tx -> m ()
  submitTx tx = do
    sendCommand $ NewTx tx

  awaitTx :: Tx -> m ()
  awaitTx tx = do
    void $ waitForHydraEvent . CustomMatcher . EventMatcher $ \case
      SnapshotConfirmed {snapshot} -> tx `elem` confirmed snapshot
      _ -> False

instance {-# OVERLAPPABLE #-} (Monad m, MonadHydra m) => MonadQueryUtxo m where
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
