{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData #-}

module HydraAuctionUtils.Monads (
  UtxoQuery (..),
  MonadTrace (..),
  MonadSubmitTx (..),
  MonadQueryUtxo (..),
  MonadNetworkId (..),
  MonadBlockchainParams (..),
  BlockchainParams (..),
  MonadCardanoClient,
  logMsg,
  submitAndAwaitTx,
  fromPlutusAddressInMonad,
  addressAndKeysForActor,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports
import Data.Set (Set)

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO
import Hydra.Cardano.Api (
  Address,
  AddressInEra,
  CardanoMode,
  EraHistory,
  NetworkId,
  PaymentKey,
  PoolId,
  ProtocolParameters,
  ShelleyAddr,
  SigningKey,
  SystemStart,
  Tx,
  TxIn,
  TxValidityLowerBound,
  TxValidityUpperBound,
  VerificationKey,
  fromPlutusAddress,
  getTxId,
  txBody,
 )

-- Plutus imports
import PlutusLedgerApi.V1 (Interval, POSIXTime)
import PlutusLedgerApi.V1.Address qualified as PlutusAddress

-- HydraAuction imports
import CardanoClient (buildAddress)
import HydraAuctionUtils.Extras.CardanoApi (networkIdToNetwork)
import HydraAuctionUtils.Fixture (Actor, keysFor)

-- MonadQueryUtxo

data UtxoQuery
  = ByAddress (Address ShelleyAddr)
  | ByTxIns [TxIn]

class Monad m => MonadQueryUtxo m where
  queryUtxo :: UtxoQuery -> m UTxO.UTxO

instance {-# OVERLAPPABLE #-} (MonadQueryUtxo m, MonadTrans t, Monad (t m)) => MonadQueryUtxo (t m) where
  queryUtxo = lift . queryUtxo

-- MonadNetworkId

class Monad m => MonadNetworkId m where
  askNetworkId :: m NetworkId

instance {-# OVERLAPPABLE #-} (MonadNetworkId m, MonadTrans t, Monad (t m)) => MonadNetworkId (t m) where
  askNetworkId = lift askNetworkId

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
  -- Should be injective, but that does not work with MonadTrace
  type TracerMessage m
  stringToMessage :: String -> TracerMessage m
  traceMessage :: TracerMessage m -> m ()

instance {-# OVERLAPPABLE #-} (MonadTrace m, MonadTrans t, Monad (t m)) => MonadTrace (t m) where
  type TracerMessage (t m) = TracerMessage m
  stringToMessage = stringToMessage @m
  traceMessage = lift . traceMessage

logMsg :: forall m. MonadTrace m => String -> m ()
logMsg = traceMessage . (stringToMessage @m)

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

instance {-# OVERLAPPABLE #-} (MonadSubmitTx m, MonadTrans t, Monad (t m)) => MonadSubmitTx (t m) where
  submitTx = lift . submitTx
  awaitTx = lift . awaitTx

-- MonadBlockchainParams

data BlockchainParams = MkBlockchainParams
  { protocolParameters :: ProtocolParameters
  , systemStart :: SystemStart
  , eraHistory :: EraHistory CardanoMode
  , stakePools :: Set PoolId
  }

class Monad m => MonadBlockchainParams m where
  queryBlockchainParams :: m BlockchainParams
  convertValidityBound :: Interval POSIXTime -> m (TxValidityLowerBound, TxValidityUpperBound)

instance (MonadBlockchainParams m, MonadTrans t, Monad (t m)) => MonadBlockchainParams (t m) where
  queryBlockchainParams = lift queryBlockchainParams
  convertValidityBound = lift . convertValidityBound

-- Complex constraint synonims

{- Why split monads at all?
   I think in some cases we may not have them for same monad.
   For example we could provide QueryUtxo but not SubmitTx
   for Delegate server clients.
-}

class
  ( MonadQueryUtxo m
  , MonadNetworkId m
  , MonadSubmitTx m
  , MonadBlockchainParams m
  ) =>
  MonadCardanoClient m
instance
  ( MonadQueryUtxo m
  , MonadNetworkId m
  , MonadSubmitTx m
  , MonadBlockchainParams m
  ) =>
  MonadCardanoClient m
