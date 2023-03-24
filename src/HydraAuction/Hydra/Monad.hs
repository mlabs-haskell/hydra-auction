{-# OPTIONS_GHC -Wno-orphans #-}
module HydraAuction.Hydra.Monad (
  MonadHydra (..),
  AwaitedHydraEvent (..),
  waitForHydraEvent,
  sendCommandAndWaitFor,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad (void)
import GHC.Natural (Natural)

-- HydraAuction imports
-- HydraAuction imports
-- HydraAuction imports
-- HydraAuction imports
import HydraAuction.Hydra.Interface (
  HydraCommand,
  HydraEvent (GetUTxOResponse, TxSeen),
  HydraEventKind,
 )
import HydraAuction.Hydra.Interface (
  HydraCommand(NewTx), HydraEvent (GetUTxOResponse),
  HydraEventKind (GetUTxOResponseKind))
import HydraAuctionUtils.Monads (MonadSubmitTx (..), MonadQueryUtxo (..), UtxoQuery (..))
import Hydra.Cardano.Api (Tx)
import qualified Cardano.Api.UTxO as UTxO

data AwaitedHydraEvent =
  Any | SpecificEvent HydraEvent | SpecificKind HydraEventKind

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

instance (Monad m, MonadHydra m) => MonadSubmitTx m where
  submitTx :: Tx -> m ()
  submitTx = sendCommand . NewTx
  awaitTx :: Tx -> m ()
  awaitTx = void . waitForHydraEvent . SpecificEvent . TxSeen

instance (Monad m, MonadHydra m) => MonadQueryUtxo m where
  queryUtxo query = do
    GetUTxOResponse utxo <-
      sendCommandAndWaitFor (SpecificKind GetUTxOResponseKind) GetUTxOResponseGetUTxO
    return $ UTxO.fromPairs $ filter predicate $ UTxO.pairs utxo
    where
      predicate (txIn, txOut) = case query of
        ByActor ac -> undefined
        ByAddress ad -> undefined
        ByTxIns txIns -> txIn `elem` txIns
