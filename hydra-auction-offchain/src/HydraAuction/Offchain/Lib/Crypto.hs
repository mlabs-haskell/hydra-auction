module HydraAuction.Offchain.Lib.Crypto (
  Address (..),
  Hash (..),
  Key (..),
  NetworkId,
  PaymentKey,
  SigningKey (..),
  Signature,
  VerificationKey (..),
  pubKeyHashToAddress,
  serialiseLovelace,
  signMessage,
  toPlutusKeyHash,
  verifySignature,
) where

import Prelude

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC8

import Cardano.Ledger.Alonzo.TxInfo qualified as Ledger
import PlutusLedgerApi.V2 qualified as PV2

import Cardano.Api.Shelley (
  Address (..),
  Hash (..),
  Key (..),
  Lovelace (..),
  NetworkId,
  PaymentCredential (..),
  PaymentKey,
  ShelleyAddr,
  SigningKey (..),
  StakeAddressReference (..),
  VerificationKey (..),
  makeShelleyAddress,
 )

import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (
  SignedDSIGN,
  signedDSIGN,
  verifySignedDSIGN,
 )

-- From Cardano API
-- Derive a verification key from a signing key.
-- getVerificationKey :: SigningKey PaymentKey -> VerificationKey PaymentKey

-- From Cardano API
-- Calculate a hash of the verification key.
-- verificationKeyHash :: VerificationKey PaymentKey -> Hash PaymentKey

type Signature = SignedDSIGN StandardCrypto ByteString

-- | Convert a cardano-api 'Hash' into a plutus 'PubKeyHash'
toPlutusKeyHash :: Hash PaymentKey -> PV2.PubKeyHash
toPlutusKeyHash (PaymentKeyHash vkh) =
  Ledger.transKeyHash vkh

-- | Convert a pubkey hash of a payment credential to an address,
-- keeping the staking address empty.
pubKeyHashToAddress :: NetworkId -> Hash PaymentKey -> Address ShelleyAddr
pubKeyHashToAddress nw pkh =
  makeShelleyAddress nw (PaymentCredentialByKey pkh) NoStakeAddress

-- | Sign an arbitrary bytestring message
signMessage ::
  SigningKey PaymentKey ->
  ByteString ->
  Signature
signMessage (PaymentSigningKey sk) = signedDSIGN @StandardCrypto sk

-- | Verify a signature for an arbitrary bytestring message.
--
-- This corresponds to PlutusCore.Crypto.Ed25519.verifyEd25519Signature_V2,
-- which is implemented using the Shelley-era cardano-crypto-class library
-- with libsodium.
-- Unfortunately, PlutusTx.Builtins.Internal.verifyEd25519Signature still
-- uses PlutusCore.Crypto.Ed25519.verifyEd25519Signature_V1,
-- which is implemented using the sunset Byron-era cardano-crypto library.
-- Hopefully there are no differences between them that would lead to
-- mismatched signatures.
verifySignature ::
  VerificationKey PaymentKey ->
  ByteString ->
  Signature ->
  Bool
verifySignature (PaymentVerificationKey vk) = verifySignedDSIGN vk

serialiseLovelace :: Lovelace -> ByteString
serialiseLovelace (Lovelace n) = BSC8.pack (show n)
