module HydraAuctionUtils.Tx.Common (
  transferAda,
  querySingleMinAdaUtxo,
  queryOrCreateSingleMinAdaUtxo,
  createMinAdaUtxo,
  utxoLovelaceValue,
  actorAdaOnlyUtxo,
  selectAdaUtxo,
) where

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
import HydraAuctionUtils.Tx.Build (minLovelace)
import HydraAuctionUtils.Tx.Utxo (filterAdaOnlyUtxo, filterNonFuelUtxo)

actorAdaOnlyUtxo ::
  forall m.
  (MonadHasActor m, MonadCardanoClient m, MonadIO m) =>
  m UTxO
actorAdaOnlyUtxo = filterNonFuelUtxo . filterAdaOnlyUtxo <$> actorTipUtxo

-- Simplest possible coin-selection algorithm
selectAdaUtxo ::
  forall m.
  (MonadHasActor m, MonadCardanoClient m, MonadIO m) =>
  Lovelace ->
  m (Maybe UTxO)
selectAdaUtxo minRequiredAmount = do
  let filterMinLovelace = UTxO.filter $
        \x -> selectLovelace (txOutValue x) >= minLovelace
  allAdaUtxo <- filterMinLovelace <$> actorAdaOnlyUtxo
  let foundEnough = utxoLovelaceValue allAdaUtxo >= minRequiredAmount
  return $ guard foundEnough >> Just allAdaUtxo

querySingleMinAdaUtxo ::
  (MonadIO m, MonadCardanoClient m, MonadTrace m, MonadFail m, MonadHasActor m) =>
  m (Maybe (TxIn, TxOut CtxUTxO))
querySingleMinAdaUtxo = do
  let filterMinLovelace = UTxO.filter $
        \x -> txOutValue x == lovelaceToValue minLovelace
  utxos <- UTxO.pairs . filterMinLovelace <$> actorAdaOnlyUtxo
  return $ case utxos of
    first : _ -> Just first
    [] -> Nothing

createMinAdaUtxo ::
  (MonadIO m, MonadCardanoClient m, MonadTrace m, MonadFail m, MonadHasActor m) =>
  m (TxIn, TxOut CtxUTxO)
createMinAdaUtxo = do
  (actorAddress, _, actorSk) <- addressAndKeys
  actorMoneyUtxo <- fromJust <$> selectAdaUtxo minLovelace

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(actorSk, actorMoneyUtxo)]
        , additionalSigners = []
        , referenceUtxo = mempty
        , witnessedUtxos = []
        , collateral = Nothing
        , outs = [minAdaOut actorAddress]
        , toMint = TxMintValueNone
        , changeAddress = actorAddress
        , validityBound = always
        }

  -- Safe, cuz we just created it (modulo concurrent txs)
  fromJust <$> querySingleMinAdaUtxo
  where
    minAdaOut actorAddress =
      TxOut
        (ShelleyAddressInEra actorAddress)
        (lovelaceToValue minLovelace)
        TxOutDatumNone
        ReferenceScriptNone

queryOrCreateSingleMinAdaUtxo ::
  (MonadIO m, MonadCardanoClient m, MonadTrace m, MonadFail m, MonadHasActor m) =>
  m (TxIn, TxOut CtxUTxO)
queryOrCreateSingleMinAdaUtxo = do
  mUtxo <- querySingleMinAdaUtxo
  maybe createMinAdaUtxo return mUtxo

utxoLovelaceValue :: UTxO.UTxO -> Lovelace
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
