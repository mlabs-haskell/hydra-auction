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
import Unsafe.Coerce (unsafeCoerce)

-- Cardano imports
import Cardano.Api (AddressInEra (..))
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports
import Hydra.Cardano.Api (Tx)
import Hydra.Cardano.Api.Prelude (TxOut (..))

-- HydraAuction imports
import HydraAuction.Hydra.Interface (
  HydraCommand (GetUTxO, NewTx),
  HydraEvent (GetUTxOResponse, TxSeen),
  HydraEventKind (GetUTxOResponseKind),
 )
import HydraAuctionUtils.Monads (MonadQueryUtxo (..), MonadSubmitTx (..), UtxoQuery (..))

data AwaitedHydraEvent
  = Any
  | SpecificEvent HydraEvent
  | SpecificKind HydraEventKind

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
  submitTx :: Tx -> m ()
  submitTx = sendCommand . NewTx
  awaitTx :: Tx -> m ()
  awaitTx = void . waitForHydraEvent . SpecificEvent . TxSeen

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
        -- FIXUP: use safe coerce
        ByAddress address -> txOutAddress == unsafeCoerce address
        ByTxIns txIns -> txIn `elem` txIns
