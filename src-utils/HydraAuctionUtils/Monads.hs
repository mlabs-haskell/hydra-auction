module HydraAuctionUtils.Monads (
  UtxoQuery (..),
  MonadTrace (..),
  MonadSubmitTx (..),
  MonadQueryUtxo (..),
  MonadNetworkId (..),
  MonadCardanoClient,
  logMsg,
  submitAndAwaitTx,
) where

-- Prelude imports
import Prelude

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO
import Hydra.Cardano.Api (
  Address,
  NetworkId,
  ShelleyAddr,
  Tx,
  TxIn,
  getTxId,
  txBody,
 )

-- HydraAuctionUtisl imports
import HydraAuctionUtils.Fixture (Actor)

-- MonadQueryUtxo

data UtxoQuery
  = ByActor Actor
  | ByAddress (Address ShelleyAddr)
  | ByTxIns [TxIn]

class Monad m => MonadQueryUtxo m where
  queryUtxo :: UtxoQuery -> m UTxO.UTxO

-- MonadNetworkId

class Monad m => MonadNetworkId m where
  askNetworkId :: m NetworkId

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
type MonadCardanoClient m =
  (MonadQueryUtxo m, MonadNetworkId m, MonadSubmitTx m)
