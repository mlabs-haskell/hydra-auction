module Plutus.Cardano.Api.Codec.Crypto (
  -- Offchain -> Onchain
  toPlutusSignature,
  toPlutusVKey,
  toPlutusVKeyHash,
  -- Onchain -> Offchain
  fromPlutusSignature,
  fromPlutusVKey,
  fromPlutusVKeyHash,
) where

import Prelude

import Data.Function ((&))

import Data.ByteString qualified as BS

import Cardano.Api.Shelley qualified as Cardano.Api

import Cardano.Crypto.DSIGN qualified as DSIGN
import Cardano.Crypto.Hash qualified as Cardano.Crypto

import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo.TxInfo
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys qualified as LG

import PlutusLedgerApi.V1.Crypto qualified as PV1.Crypto

import PlutusTx.Prelude qualified as Plutus

-- -------------------------------------------------------------------------
-- Signature
-- -------------------------------------------------------------------------
-- PlutusCore.Crypto.Ed25519.verifyEd25519Signature_V2
-- deserializes its bytestring signature argument
-- with DSIGN.rawDeserialiseSigDSIGN.
-- Although PlutusTx.Builtins.verifyEd25519Signature
-- uses PlutusCore.Crypto.Ed25519.verifyEd25519Signature_V1
-- instead, the hope is that they serialize the signature
-- in the same way...
toPlutusSignature ::
  LG.SignedDSIGN StandardCrypto BS.ByteString ->
  Plutus.BuiltinByteString
toPlutusSignature (DSIGN.SignedDSIGN sigDSIGN) =
  sigDSIGN
    & DSIGN.rawSerialiseSigDSIGN
    & Plutus.toBuiltin

fromPlutusSignature ::
  Plutus.BuiltinByteString ->
  Maybe (LG.SignedDSIGN StandardCrypto BS.ByteString)
fromPlutusSignature pSignatureBytes = do
  sigDSIGN <-
    pSignatureBytes
      & Plutus.fromBuiltin
      & DSIGN.rawDeserialiseSigDSIGN
  pure $ DSIGN.SignedDSIGN sigDSIGN

-- -------------------------------------------------------------------------
-- Verification Keys
-- -------------------------------------------------------------------------
toPlutusVKey ::
  Cardano.Api.VerificationKey Cardano.Api.PaymentKey ->
  Plutus.BuiltinByteString
toPlutusVKey cVKey =
  cVKey
    & Cardano.Api.serialiseToRawBytes
    & Plutus.toBuiltin

fromPlutusVKey ::
  Plutus.BuiltinByteString ->
  Maybe (Cardano.Api.VerificationKey Cardano.Api.PaymentKey)
fromPlutusVKey pVKey =
  pVKey
    & Plutus.fromBuiltin
    & Cardano.Api.deserialiseFromRawBytes
      (Cardano.Api.AsVerificationKey Cardano.Api.AsPaymentKey)
    & either (const Nothing) Just

-- -------------------------------------------------------------------------
-- Verification key hashes
-- -------------------------------------------------------------------------
toPlutusVKeyHash ::
  Cardano.Api.Hash Cardano.Api.PaymentKey ->
  PV1.Crypto.PubKeyHash
toPlutusVKeyHash (Cardano.Api.PaymentKeyHash cVKeyHash) =
  Alonzo.TxInfo.transKeyHash cVKeyHash

fromPlutusVKeyHash ::
  PV1.Crypto.PubKeyHash ->
  Maybe (Cardano.Api.Hash Cardano.Api.PaymentKey)
fromPlutusVKeyHash pVKeyHash = do
  lVKeyHash <- transKeyHashInverse pVKeyHash
  pure $ Cardano.Api.PaymentKeyHash lVKeyHash

-- This is the inverse of Alonzo.TxInfo.transKeyHash,
-- except when the Plutus.PubKeyHash bytestring cannot be
-- decoded as a valid key hash.
transKeyHashInverse ::
  PV1.Crypto.PubKeyHash ->
  Maybe (LG.KeyHash 'LG.Payment StandardCrypto)
transKeyHashInverse (PV1.Crypto.PubKeyHash pPubKeyHashBytes) = do
  hash <-
    pPubKeyHashBytes
      & Plutus.fromBuiltin
      & Cardano.Crypto.hashFromBytes
  pure $ LG.KeyHash hash
