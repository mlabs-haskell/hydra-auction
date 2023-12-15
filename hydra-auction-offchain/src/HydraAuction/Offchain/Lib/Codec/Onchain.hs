module HydraAuction.Offchain.Lib.Codec.Onchain (
  -- Offchain -> Onchain
  toPlutusAssetId,
  toPlutusAssetName,
  toPlutusBytestring,
  toPlutusLovelace,
  toPlutusPolicyId,
  toPlutusSignature,
  toPlutusTextUtf8,
  toPlutusUTCTimeMilli,
  toPlutusValue,
  toPlutusVKey,
  toPlutusVKeyHash,
  -- Onchain -> Offchain
  fromPlutusAssetId,
  fromPlutusAssetName,
  fromPlutusBytestring,
  fromPlutusLovelace,
  fromPlutusPolicyId,
  fromPlutusSignature,
  fromPlutusTextUtf8,
  fromPlutusUTCTimeMilli,
  fromPlutusValue,
  fromPlutusVKey,
  fromPlutusVKeyHash,
) where

import Prelude

import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as Short
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding

import Cardano.Api.Shelley qualified as Cardano.Api

import Cardano.Crypto.DSIGN qualified as DSIGN
import Cardano.Crypto.Hash qualified as Cardano.Crypto

import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo.TxInfo
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Hashes qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Cardano.Ledger.Mary.Value qualified as Mary

import PlutusLedgerApi.V1.Crypto qualified as Plutus
import PlutusLedgerApi.V1.Time qualified as Plutus
import PlutusLedgerApi.V1.Value qualified as Plutus
import PlutusTx.Prelude qualified as Plutus

import HydraAuction.Offchain.Lib.Time qualified as AuctionTime

-- -------------------------------------------------------------------------
-- Time
-- -------------------------------------------------------------------------
toPlutusUTCTimeMilli ::
  AuctionTime.UTCTimeMilli ->
  Plutus.POSIXTime
toPlutusUTCTimeMilli =
  Plutus.POSIXTime
    . AuctionTime.posixTimeMilliToInteger
    . AuctionTime.utcTimeMilliToPOSIXMilli

fromPlutusUTCTimeMilli ::
  Plutus.POSIXTime ->
  AuctionTime.UTCTimeMilli
fromPlutusUTCTimeMilli =
  AuctionTime.posixMilliToUTCMilli
    . AuctionTime.integerToPOSIXTimeMilli
    . Plutus.getPOSIXTime

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
  Ledger.SignedDSIGN StandardCrypto BS.ByteString ->
  Plutus.BuiltinByteString
toPlutusSignature (DSIGN.SignedDSIGN sig) =
  Plutus.toBuiltin $ DSIGN.rawSerialiseSigDSIGN sig

fromPlutusSignature ::
  Plutus.BuiltinByteString ->
  Maybe (Ledger.SignedDSIGN StandardCrypto BS.ByteString)
fromPlutusSignature bbs =
  DSIGN.SignedDSIGN
    <$> DSIGN.rawDeserialiseSigDSIGN (Plutus.fromBuiltin bbs)

-- -------------------------------------------------------------------------
-- Verification Keys
-- -------------------------------------------------------------------------
toPlutusVKey ::
  Cardano.Api.VerificationKey Cardano.Api.PaymentKey ->
  Plutus.BuiltinByteString
toPlutusVKey = Plutus.toBuiltin . Cardano.Api.serialiseToRawBytes

fromPlutusVKey ::
  Plutus.BuiltinByteString ->
  Maybe (Cardano.Api.VerificationKey Cardano.Api.PaymentKey)
fromPlutusVKey =
  either (const Nothing) Just
    . Cardano.Api.deserialiseFromRawBytes
      (Cardano.Api.AsVerificationKey Cardano.Api.AsPaymentKey)
    . Plutus.fromBuiltin

-- -------------------------------------------------------------------------
-- Verification key hashes
-- -------------------------------------------------------------------------
toPlutusVKeyHash ::
  Cardano.Api.Hash Cardano.Api.PaymentKey ->
  Plutus.PubKeyHash
toPlutusVKeyHash (Cardano.Api.PaymentKeyHash kh) = Alonzo.TxInfo.transKeyHash kh

fromPlutusVKeyHash ::
  Plutus.PubKeyHash ->
  Maybe (Cardano.Api.Hash Cardano.Api.PaymentKey)
fromPlutusVKeyHash pkh = Cardano.Api.PaymentKeyHash <$> transKeyHashInverse pkh

-- This is the inverse of Alonzo.TxInfo.transKeyHash,
-- except when the Plutus.PubKeyHash bytestring cannot be
-- decoded as a valid key hash.
transKeyHashInverse ::
  Plutus.PubKeyHash ->
  Maybe (Ledger.KeyHash 'Ledger.Payment StandardCrypto)
transKeyHashInverse (Plutus.PubKeyHash bbs) =
  Ledger.KeyHash <$> Cardano.Crypto.hashFromBytes (Plutus.fromBuiltin bbs)

-- -------------------------------------------------------------------------
-- Values
-- -------------------------------------------------------------------------
toPlutusValue :: Cardano.Api.Value -> Plutus.Value
toPlutusValue cValue = foldMap convert cValueList
  where
    cValueList = Cardano.Api.valueToList cValue
    convert (assetId, Cardano.Api.Quantity n) =
      Plutus.assetClassValue (toPlutusAssetId assetId) n

fromPlutusValue :: Plutus.Value -> Maybe Cardano.Api.Value
fromPlutusValue pValue =
  Cardano.Api.valueFromList
    <$> traverse convert (Plutus.flattenValue pValue)
  where
    convert (cs, tn, n) = do
      ai <- fromPlutusAssetId $ Plutus.assetClass cs tn
      pure (ai, Cardano.Api.Quantity n)

toPlutusAssetId :: Cardano.Api.AssetId -> Plutus.AssetClass
toPlutusAssetId Cardano.Api.AdaAssetId =
  Plutus.AssetClass (Plutus.adaSymbol, Plutus.adaToken)
toPlutusAssetId (Cardano.Api.AssetId p n) =
  Plutus.AssetClass (toPlutusPolicyId p, toPlutusAssetName n)

fromPlutusAssetId :: Plutus.AssetClass -> Maybe Cardano.Api.AssetId
fromPlutusAssetId (Plutus.AssetClass (cs, tn))
  | cs == Plutus.adaSymbol && tn == Plutus.adaToken =
      pure Cardano.Api.AdaAssetId
  | otherwise =
      Cardano.Api.AssetId
        <$> fromPlutusPolicyId cs
        <*> pure (fromPlutusAssetName tn)

toPlutusPolicyId :: Cardano.Api.PolicyId -> Plutus.CurrencySymbol
toPlutusPolicyId (Cardano.Api.PolicyId sh) =
  Alonzo.TxInfo.transPolicyID $ Mary.PolicyID lsh
  where
    lsh = Cardano.Api.toShelleyScriptHash sh

fromPlutusPolicyId :: Plutus.CurrencySymbol -> Maybe Cardano.Api.PolicyId
fromPlutusPolicyId p = do
  Mary.PolicyID lsh <- transPolicyIDInverse p
  pure $ Cardano.Api.PolicyId $ Cardano.Api.fromShelleyScriptHash lsh

-- This is the inverse of Alonzo.TxInfo.transPolicyID,
-- except when the Plutus.CurrencySymbol bytestring cannot be
-- decoded as a valid minting policy script hash.
transPolicyIDInverse ::
  Plutus.CurrencySymbol ->
  Maybe (Mary.PolicyID StandardCrypto)
transPolicyIDInverse (Plutus.CurrencySymbol bbs) =
  Mary.PolicyID . Ledger.ScriptHash
    <$> Cardano.Crypto.hashFromBytes (Plutus.fromBuiltin bbs)

toPlutusAssetName :: Cardano.Api.AssetName -> Plutus.TokenName
toPlutusAssetName (Cardano.Api.AssetName an) =
  Alonzo.TxInfo.transAssetName $ toMaryAssetName an

-- The (fromMaryAssetName . toMaryAssetName) roundtrip is wasteful,
-- but at least the compiler will complain
-- if Cardano, Ledger, or Plutus types change internally.
fromPlutusAssetName :: Plutus.TokenName -> Cardano.Api.AssetName
fromPlutusAssetName =
  Cardano.Api.AssetName . fromMaryAssetName . transAssetNameInverse
  where
    transAssetNameInverse (Plutus.TokenName bbs) =
      toMaryAssetName $ Plutus.fromBuiltin bbs

toMaryAssetName :: BS.ByteString -> Mary.AssetName
toMaryAssetName = Mary.AssetName . Short.toShort

fromMaryAssetName :: Mary.AssetName -> BS.ByteString
fromMaryAssetName (Mary.AssetName an) = Short.fromShort an

-- -------------------------------------------------------------------------
-- Lovelace integers
-- -------------------------------------------------------------------------
toPlutusLovelace :: Cardano.Api.Lovelace -> Integer
toPlutusLovelace (Cardano.Api.Lovelace n) = n

fromPlutusLovelace :: Integer -> Cardano.Api.Lovelace
fromPlutusLovelace = Cardano.Api.Lovelace

-- -------------------------------------------------------------------------
-- Text
-- -------------------------------------------------------------------------
toPlutusTextUtf8 :: Text.Text -> Plutus.BuiltinByteString
toPlutusTextUtf8 = Plutus.toBuiltin . Text.Encoding.encodeUtf8

fromPlutusTextUtf8 :: Plutus.BuiltinByteString -> Maybe Text.Text
fromPlutusTextUtf8 =
  either (const Nothing) Just
    . Text.Encoding.decodeUtf8'
    . Plutus.fromBuiltin

-- -------------------------------------------------------------------------
-- Bytestring
-- -------------------------------------------------------------------------
toPlutusBytestring :: BS.ByteString -> Plutus.BuiltinByteString
toPlutusBytestring = Plutus.toBuiltin

fromPlutusBytestring :: Plutus.BuiltinByteString -> BS.ByteString
fromPlutusBytestring = Plutus.fromBuiltin
