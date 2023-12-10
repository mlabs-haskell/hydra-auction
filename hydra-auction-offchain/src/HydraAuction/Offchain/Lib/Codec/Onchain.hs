module HydraAuction.Offchain.Lib.Codec.Onchain (
  -- Offchain -> Onchain
  toPlutusAddress,
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
  fromPlutusAddress,
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
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding

import Cardano.Api.Shelley qualified as Cardano.Api

import Cardano.Crypto.DSIGN qualified as DSIGN
import Cardano.Crypto.Hash qualified as Cardano.Crypto

import Cardano.Ledger.Address qualified as Ledger
import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo.TxInfo
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Hashes qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Cardano.Ledger.Mary.Value qualified as Mary

import PlutusLedgerApi.V1.Address qualified as PV1.Address
import PlutusLedgerApi.V1.Credential qualified as PV1.Credential
import PlutusLedgerApi.V1.Crypto qualified as PV1.Crypto
import PlutusLedgerApi.V1.Scripts qualified as PV1.Scripts
import PlutusLedgerApi.V1.Time qualified as PV1.Time
import PlutusLedgerApi.V1.Value qualified as PV1.Value

-- import PlutusLedgerApi.V2 qualified as PV2

import PlutusTx.Prelude qualified as Plutus

import HydraAuction.Offchain.Lib.Time qualified as AuctionTime

-- -------------------------------------------------------------------------
-- Time
-- -------------------------------------------------------------------------
toPlutusUTCTimeMilli ::
  AuctionTime.UTCTimeMilli ->
  PV1.Time.POSIXTime
toPlutusUTCTimeMilli =
  PV1.Time.POSIXTime
    . AuctionTime.posixTimeMilliToInteger
    . AuctionTime.utcTimeMilliToPOSIXMilli

fromPlutusUTCTimeMilli ::
  PV1.Time.POSIXTime ->
  AuctionTime.UTCTimeMilli
fromPlutusUTCTimeMilli =
  AuctionTime.posixMilliToUTCMilli
    . AuctionTime.integerToPOSIXTimeMilli
    . PV1.Time.getPOSIXTime

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
  PV1.Crypto.PubKeyHash
toPlutusVKeyHash (Cardano.Api.PaymentKeyHash kh) = Alonzo.TxInfo.transKeyHash kh

fromPlutusVKeyHash ::
  PV1.Crypto.PubKeyHash ->
  Maybe (Cardano.Api.Hash Cardano.Api.PaymentKey)
fromPlutusVKeyHash pkh = Cardano.Api.PaymentKeyHash <$> transKeyHashInverse pkh

-- This is the inverse of Alonzo.TxInfo.transKeyHash,
-- except when the Plutus.PubKeyHash bytestring cannot be
-- decoded as a valid key hash.
transKeyHashInverse ::
  PV1.Crypto.PubKeyHash ->
  Maybe (Ledger.KeyHash 'Ledger.Payment StandardCrypto)
transKeyHashInverse (PV1.Crypto.PubKeyHash bbs) =
  Ledger.KeyHash <$> Cardano.Crypto.hashFromBytes (Plutus.fromBuiltin bbs)

-- -------------------------------------------------------------------------
-- Shelley-based addresses (i.e. non-Byron)
-- -------------------------------------------------------------------------
toPlutusAddress ::
  Cardano.Api.IsShelleyBasedEra era =>
  Cardano.Api.AddressInEra era ->
  PV1.Address.Address
toPlutusAddress =
  Maybe.fromMaybe (error "Impossible Byron address here")
    . Alonzo.TxInfo.transAddr
    . Cardano.Api.toShelleyAddr

fromPlutusAddress ::
  Cardano.Api.IsShelleyBasedEra era =>
  Cardano.Api.NetworkId ->
  PV1.Address.Address ->
  Maybe (Cardano.Api.AddressInEra era)
fromPlutusAddress net =
  fmap Cardano.Api.fromShelleyAddrIsSbe
    . transAddrInverse (Cardano.Api.toShelleyNetwork net)

transAddrInverse ::
  Ledger.Network ->
  PV1.Address.Address ->
  Maybe (Ledger.Addr StandardCrypto)
transAddrInverse net (PV1.Address.Address credential stakeRef) = do
  lCredential <- transCredInverse credential
  lStakeRef <- transStakeReferenceInverse stakeRef
  pure $ Ledger.Addr net lCredential lStakeRef

-- This is the inverse of Alonzo.TxInfo.transCredInverse,
-- except when the Plutus.PubKeyHash or Plutus.ScriptHash bytestring
-- cannot be decoded as valid hashes.
transCredInverse ::
  PV1.Credential.Credential ->
  Maybe (Ledger.Credential kr StandardCrypto)
transCredInverse
  (PV1.Credential.PubKeyCredential (PV1.Crypto.PubKeyHash bbs)) =
    Ledger.KeyHashObj . Ledger.KeyHash
      <$> Cardano.Crypto.hashFromBytes (Plutus.fromBuiltin bbs)
transCredInverse
  (PV1.Credential.ScriptCredential (PV1.Scripts.ScriptHash bbs)) =
    Ledger.ScriptHashObj . Ledger.ScriptHash
      <$> Cardano.Crypto.hashFromBytes (Plutus.fromBuiltin bbs)

-- This is the inverse of Alonzo.TxInfo.transStakeReference,
-- except when there are deserialization errors from Plutus.
transStakeReferenceInverse ::
  Maybe PV1.Credential.StakingCredential ->
  Maybe (Ledger.StakeReference StandardCrypto)
transStakeReferenceInverse =
  Maybe.maybe (Just Ledger.StakeRefNull) transStakeReferenceInverse'

-- This helper function isolates the errors that can occur
-- when deserializing a Plutus.StakingCredential from Plutus.
transStakeReferenceInverse' ::
  PV1.Credential.StakingCredential ->
  Maybe (Ledger.StakeReference StandardCrypto)
transStakeReferenceInverse' (PV1.Credential.StakingHash credential) =
  Ledger.StakeRefBase
    <$> transCredInverse credential
transStakeReferenceInverse' (PV1.Credential.StakingPtr pSlot pTxIx pCertIx) = do
  lTxIx <- Ledger.txIxFromIntegral @Integer $ fromInteger pTxIx
  lCertIx <- Ledger.certIxFromIntegral @Integer $ fromInteger pCertIx
  let lSlot = fromIntegral pSlot
  pure $
    Ledger.StakeRefPtr $
      Ledger.Ptr (Ledger.SlotNo lSlot) lTxIx lCertIx

-- -------------------------------------------------------------------------
-- Values
-- -------------------------------------------------------------------------
toPlutusValue :: Cardano.Api.Value -> PV1.Value.Value
toPlutusValue cValue = foldMap convert cValueList
  where
    cValueList = Cardano.Api.valueToList cValue
    convert (assetId, Cardano.Api.Quantity n) =
      PV1.Value.assetClassValue (toPlutusAssetId assetId) n

fromPlutusValue :: PV1.Value.Value -> Maybe Cardano.Api.Value
fromPlutusValue pValue =
  Cardano.Api.valueFromList
    <$> traverse convert (PV1.Value.flattenValue pValue)
  where
    convert (cs, tn, n) = do
      ai <- fromPlutusAssetId $ PV1.Value.assetClass cs tn
      pure (ai, Cardano.Api.Quantity n)

toPlutusAssetId :: Cardano.Api.AssetId -> PV1.Value.AssetClass
toPlutusAssetId Cardano.Api.AdaAssetId =
  PV1.Value.AssetClass (PV1.Value.adaSymbol, PV1.Value.adaToken)
toPlutusAssetId (Cardano.Api.AssetId p n) =
  PV1.Value.AssetClass (toPlutusPolicyId p, toPlutusAssetName n)

fromPlutusAssetId :: PV1.Value.AssetClass -> Maybe Cardano.Api.AssetId
fromPlutusAssetId (PV1.Value.AssetClass (cs, tn))
  | cs == PV1.Value.adaSymbol && tn == PV1.Value.adaToken =
      pure Cardano.Api.AdaAssetId
  | otherwise =
      Cardano.Api.AssetId
        <$> fromPlutusPolicyId cs
        <*> pure (fromPlutusAssetName tn)

toPlutusPolicyId :: Cardano.Api.PolicyId -> PV1.Value.CurrencySymbol
toPlutusPolicyId (Cardano.Api.PolicyId sh) =
  Alonzo.TxInfo.transPolicyID $ Mary.PolicyID lsh
  where
    lsh = Cardano.Api.toShelleyScriptHash sh

fromPlutusPolicyId :: PV1.Value.CurrencySymbol -> Maybe Cardano.Api.PolicyId
fromPlutusPolicyId p = do
  Mary.PolicyID lsh <- transPolicyIDInverse p
  pure $ Cardano.Api.PolicyId $ Cardano.Api.fromShelleyScriptHash lsh

-- This is the inverse of Alonzo.TxInfo.transPolicyID,
-- except when the Plutus.CurrencySymbol bytestring cannot be
-- decoded as a valid minting policy script hash.
transPolicyIDInverse ::
  PV1.Value.CurrencySymbol ->
  Maybe (Mary.PolicyID StandardCrypto)
transPolicyIDInverse (PV1.Value.CurrencySymbol bbs) =
  Mary.PolicyID . Ledger.ScriptHash
    <$> Cardano.Crypto.hashFromBytes (Plutus.fromBuiltin bbs)

toPlutusAssetName :: Cardano.Api.AssetName -> PV1.Value.TokenName
toPlutusAssetName (Cardano.Api.AssetName an) =
  Alonzo.TxInfo.transAssetName $ toMaryAssetName an

-- The (fromMaryAssetName . toMaryAssetName) roundtrip is wasteful,
-- but at least the compiler will complain
-- if Cardano, Ledger, or Plutus types change internally.
fromPlutusAssetName :: PV1.Value.TokenName -> Cardano.Api.AssetName
fromPlutusAssetName =
  Cardano.Api.AssetName . fromMaryAssetName . transAssetNameInverse
  where
    transAssetNameInverse (PV1.Value.TokenName bbs) =
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
