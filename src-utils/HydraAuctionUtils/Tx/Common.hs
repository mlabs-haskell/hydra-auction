module HydraAuctionUtils.Tx.Common (transferAda) where

-- Prelude imports
import Prelude

-- Haskell imports
import Data.Maybe ()

-- Hydra imports
import Hydra.Cardano.Api (
  Lovelace,
  Tx,
  lovelaceToValue,
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
import HydraAuctionUtils.L1.Runner (L1Runner)
import HydraAuctionUtils.Monads (addressAndKeysForActor)
import HydraAuctionUtils.Monads.Actors (actorTipUtxo, addressAndKeys)
import HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoSubmitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.Utxo ()

transferAda :: Actor -> Marked -> Lovelace -> L1Runner Tx
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
      , validityBound = (Nothing, Nothing)
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
