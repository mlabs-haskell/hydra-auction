module HydraAuctionUtils.Tx.Common (transferAda, selectAdaUtxo) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Plutus imports
import PlutusLedgerApi.V2 (always)

-- Cardano imports

import Cardano.Api.UTxO (UTxO)
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports
import Hydra.Cardano.Api (
  Lovelace,
  Tx,
  lovelaceToValue,
  selectLovelace,
  txOutValue,
  pattern ReferenceScriptNone,
  pattern ShelleyAddressInEra,
  pattern TxMintValueNone,
  pattern TxOut,
  pattern TxOutDatumHash,
  pattern TxOutDatumNone,
 )
import Hydra.Chain.Direct.Util (markerDatumHash)
import Hydra.Cluster.Faucet (Marked (..))

-- Hydra auction imports
import HydraAuctionUtils.Fixture (Actor)
import HydraAuctionUtils.Monads (
  MonadCardanoClient,
  MonadTrace,
  addressAndKeysForActor,
 )
import HydraAuctionUtils.Monads.Actors (
  MonadHasActor (..),
  actorTipUtxo,
  addressAndKeys,
 )
import HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoSubmitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.Utxo (filterAdaOnlyUtxo, filterNonFuelUtxo)

-- Simplest possible coin-selection algorithm
selectAdaUtxo ::
  forall m.
  (MonadHasActor m, MonadCardanoClient m, MonadIO m) =>
  Lovelace ->
  m (Maybe UTxO)
selectAdaUtxo minRequiredAmount = do
  allAdaUtxo <- filterNonFuelUtxo . filterAdaOnlyUtxo <$> actorTipUtxo
  let foundEnough = utxoLovelaceValue allAdaUtxo >= minRequiredAmount
  return $ guard foundEnough >> Just allAdaUtxo
  where
    utxoLovelaceValue =
      sum . fmap (selectLovelace . txOutValue . snd) . UTxO.pairs

transferAda ::
  forall m.
  (MonadFail m, MonadTrace m, MonadHasActor m, MonadCardanoClient m, MonadIO m) =>
  Actor ->
  Marked ->
  Lovelace ->
  m Tx
transferAda actorTo marked amount = do
  moneyUtxo <- actorTipUtxo
  (fromAddress, _, fromSk) <- addressAndKeys
  (toAddress, _, _) <- addressAndKeysForActor actorTo

  autoSubmitAndAwaitTx $
    AutoCreateParams
      { signedUtxos = [(fromSk, moneyUtxo)]
      , additionalSigners = []
      , referenceUtxo = mempty
      , collateral = Nothing
      , witnessedUtxos = []
      , outs = [txOut toAddress]
      , toMint = TxMintValueNone
      , changeAddress = fromAddress
      , validityBound = always
      }
  where
    txOut toAddress =
      TxOut
        (ShelleyAddressInEra toAddress)
        (lovelaceToValue amount)
        theOutputDatum
        ReferenceScriptNone

    theOutputDatum = case marked of
      Fuel -> TxOutDatumHash markerDatumHash
      Normal -> TxOutDatumNone
