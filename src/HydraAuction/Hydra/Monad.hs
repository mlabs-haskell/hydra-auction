{-# OPTIONS_GHC -Wno-orphans #-}

module HydraAuction.Hydra.Monad (
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
import Hydra.Cardano.Api (Address (ByronAddress, ShelleyAddress), Tx, pattern TxOut)

-- HydraAuction imports
import HydraAuction.Hydra.Interface (
  HydraCommand (GetUTxO, NewTx),
  HydraEvent (..),
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
  waitForHydraEvent' :: Natural -> AwaitedHydraEvent -> m HydraEvent

defaultTimeout :: Natural
defaultTimeout = 30

waitForHydraEvent :: MonadHydra m => AwaitedHydraEvent -> m HydraEvent
waitForHydraEvent = waitForHydraEvent' defaultTimeout

sendCommandAndWaitFor :: MonadHydra m => AwaitedHydraEvent -> HydraCommand -> m HydraEvent
sendCommandAndWaitFor awaitedSpec command = do
  sendCommand command
  waitForHydraEvent awaitedSpec

-- FIXME: this does not work without `Monad m`, do not know why
instance {-# OVERLAPPABLE #-} (Monad m, MonadHydra m) => MonadSubmitTx m where
  -- FIXME: handle TxValid/TxInvalid
  submitTx :: Tx -> m ()
  submitTx tx = do
    sendCommand $ NewTx tx

  awaitTx :: Tx -> m ()
  awaitTx tx = do
    void $ waitForHydraEvent . CustomMatcher . EventMatcher $ \case
      SnapshotConfirmed {txs} -> tx `elem` txs
      _ -> False

instance {-# OVERLAPPABLE #-} (Monad m, MonadHydra m) => MonadQueryUtxo m where
  queryUtxo query = do
    response <-
      sendCommandAndWaitFor (SpecificKind GetUTxOResponseKind) GetUTxO
    let utxo = case response of
          GetUTxOResponse utxo' -> utxo'
          _ -> error "Impossible happened: incorrect response type awaited"
    return $ UTxO.fromPairs $ filter predicate $ UTxO.pairs utxo
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
