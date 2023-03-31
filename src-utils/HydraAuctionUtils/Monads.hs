{-# LANGUAGE StrictData #-}

module HydraAuctionUtils.Monads (
  UtxoQuery (..),
  MonadTrace (..),
  MonadSubmitTx (..),
  MonadQueryUtxo (..),
  MonadNetworkId (..),
  MonadCardanoClient,
  logMsg,
  submitAndAwaitTx,
  fromPlutusAddressInMonad,
  addressAndKeysForActor,
) where

-- Prelude imports
import Prelude

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO
import Hydra.Cardano.Api (
  Address,
  AddressInEra,
  NetworkId,
  PaymentKey,
  ShelleyAddr,
  SigningKey,
  Tx,
  TxIn,
  VerificationKey,
  fromPlutusAddress,
  getTxId,
  txBody,
 )

-- Plutus imports
import Plutus.V1.Ledger.Address qualified as PlutusAddress

-- HydraAuction imports
import CardanoClient (buildAddress)
import Control.Monad.IO.Class (MonadIO (liftIO))
import HydraAuctionUtils.Extras.CardanoApi (networkIdToNetwork)
import HydraAuctionUtils.Fixture (Actor, keysFor)

-- MonadQueryUtxo

data UtxoQuery
  = ByAddress (Address ShelleyAddr)
  | ByTxIns [TxIn]

class Monad m => MonadQueryUtxo m where
  queryUtxo :: UtxoQuery -> m UTxO.UTxO

-- MonadNetworkId

class Monad m => MonadNetworkId m where
  askNetworkId :: m NetworkId

fromPlutusAddressInMonad ::
  MonadNetworkId m => PlutusAddress.Address -> m AddressInEra
fromPlutusAddressInMonad address = do
  networkId <- askNetworkId
  let network = networkIdToNetwork networkId
  return $
    fromPlutusAddress network address

addressAndKeysForActor ::
  (MonadNetworkId m, MonadIO m) =>
  Actor ->
  m
    ( Address ShelleyAddr
    , VerificationKey PaymentKey
    , SigningKey PaymentKey
    )
addressAndKeysForActor actor = do
  networkId' <- askNetworkId
  (actorVk, actorSk) <- liftIO $ keysFor actor
  let actorAddress = buildAddress actorVk networkId'
  pure (actorAddress, actorVk, actorSk)

-- MonadTrace

class Monad m => MonadTrace m where
  type TracerMessage m = mt | mt -> m
  stringToMessage :: String -> TracerMessage m
  traceMessage :: TracerMessage m -> m ()

logMsg :: MonadTrace m => String -> m ()
logMsg = traceMessage . stringToMessage

-- MonadSubmitTx

class Monad m => MonadSubmitTx m where
  submitTx :: Tx -> m ()
  awaitTx :: Tx -> m ()

submitAndAwaitTx :: (MonadSubmitTx m, MonadTrace m) => Tx -> m ()
submitAndAwaitTx tx = do
  submitTx tx
  logMsg "Submited"
  awaitTx tx
  logMsg $ "Created Tx id: " <> show (getTxId $ txBody tx)

-- Complex constraint synonims

{- Why split monads at all?
   I think in some cases we may not have them for same monad.
   For example we could provide QueryUtxo but not SubmitTx
   for Delegate server clients.
-}
class (MonadQueryUtxo m, MonadNetworkId m, MonadSubmitTx m) => MonadCardanoClient m
instance (MonadQueryUtxo m, MonadNetworkId m, MonadSubmitTx m) => MonadCardanoClient m
