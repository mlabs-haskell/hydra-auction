module HydraAuction.Tx (createAuction) where

import CardanoClient (
  -- QueryPoint (QueryTip),
  buildAddress,
  -- queryUTxO,
  sign,
 )
import Hydra.Cardano.Api (
  AddressInEra,
  CtxTx,
  Hash (..),
  Key (..),
  NetworkId,
  PaymentKey,
  PlutusScriptV2,
  ScriptData,
  SigningKey,
  TxIn (..),
  TxOut,
  UTxO,
  fromPlutusData,
  hashScriptData,
  lovelaceToValue,
  mkScriptAddress,
  shelleyAddressInEra,
  pattern ReferenceScriptNone,
  pattern TxOut,
  pattern TxOutDatumHash,
 )
import Hydra.Cardano.Api.PlutusScript (fromPlutusScript)
import Hydra.Chain.CardanoClient (
  buildTransaction,
  submitTransaction,
 )
import Hydra.Prelude
import HydraAuction.OnChain
import HydraAuction.Types
import Plutus.V1.Ledger.Api (Data, Script, getValidator, toBuiltinData, toData)

createAuction :: AuctionTerms -> NetworkId -> FilePath -> SigningKey PaymentKey -> VerificationKey PaymentKey -> [TxIn] -> UTxO -> IO ()
createAuction at nid sock mySkey myVkey collateral utxos = do
  let escrow :: Script
      escrow = getValidator $ escrowValidator at

      atDatum :: Data
      atDatum = toData $ toBuiltinData at

      atDatumHash :: Hash ScriptData
      atDatumHash =
        hashScriptData $ fromPlutusData atDatum

      out :: TxOut CtxTx
      out =
        TxOut
          (mkScriptAddress @PlutusScriptV2 nid $ fromPlutusScript escrow)
          (lovelaceToValue 100000)
          (TxOutDatumHash atDatumHash)
          ReferenceScriptNone

      change :: AddressInEra
      change = shelleyAddressInEra (buildAddress myVkey nid)

      spendUtxos :: UTxO
      spendUtxos = utxos

  buildTransaction nid sock change spendUtxos collateral [out] >>= \case
    Left _ -> error "unable to build tx"
    Right txBody -> do
      submitTransaction nid sock (sign mySkey txBody)
