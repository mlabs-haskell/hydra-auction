module Plutus.Cardano.Api.Codec.Address (
  -- Offchain -> Onchain
  toPlutusAddress,
  -- Onchain -> Offchain
  fromPlutusAddress,
  -- Offchain script -> Offchain script hash
) where

import Prelude

import Data.Function ((&))
import Data.Maybe (fromMaybe)

import Cardano.Api.Shelley qualified as CA

import Cardano.Crypto.Hash qualified as Cardano.Crypto

import Cardano.Ledger.Address qualified as LG
import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo.TxInfo
import Cardano.Ledger.BaseTypes qualified as LG
import Cardano.Ledger.Credential qualified as LG
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Hashes qualified as LG
import Cardano.Ledger.Keys qualified as LG

import PlutusLedgerApi.V1.Address qualified as PV1.Address
import PlutusLedgerApi.V1.Credential qualified as PV1.Credential
import PlutusLedgerApi.V1.Crypto qualified as PV1.Crypto
import PlutusLedgerApi.V1.Scripts qualified as PV1.Scripts

import PlutusTx.Prelude qualified as Plutus

-- -------------------------------------------------------------------------
-- Shelley-based addresses (i.e. non-Byron)
-- -------------------------------------------------------------------------
toPlutusAddress ::
  CA.AddressInEra era ->
  PV1.Address.Address
toPlutusAddress cAddressInEra =
  cAddressInEra
    & CA.toShelleyAddr
    & Alonzo.TxInfo.transAddr
    & fromMaybe (error "Unexpected Byron address encountered")

fromPlutusAddress ::
  CA.IsShelleyBasedEra era =>
  CA.NetworkId ->
  PV1.Address.Address ->
  Maybe (CA.AddressInEra era)
fromPlutusAddress net pAddress = do
  lAddress <-
    pAddress
      & transAddrInverse (CA.toShelleyNetwork net)
  pure $ CA.fromShelleyAddrIsSbe lAddress

transAddrInverse ::
  LG.Network ->
  PV1.Address.Address ->
  Maybe (LG.Addr StandardCrypto)
transAddrInverse net (PV1.Address.Address credential stakeRef) = do
  lCredential <- transCredInverse credential
  lStakeRef <- transStakeReferenceInverse stakeRef
  pure $ LG.Addr net lCredential lStakeRef

-- This is the inverse of Alonzo.TxInfo.transCredInverse,
-- except when the Plutus.PubKeyHash or Plutus.ScriptHash bytestring
-- cannot be decoded as valid hashes.
transCredInverse ::
  PV1.Credential.Credential ->
  Maybe (LG.Credential kr StandardCrypto)
transCredInverse (PV1.Credential.PubKeyCredential pVKeyHash) = do
  let PV1.Crypto.PubKeyHash pVKeyHashBytes = pVKeyHash
  lHash <-
    pVKeyHashBytes
      & Plutus.fromBuiltin
      & Cardano.Crypto.hashFromBytes
  pure $
    lHash
      & LG.KeyHash
      & LG.KeyHashObj
--
transCredInverse (PV1.Credential.ScriptCredential pScriptHash) = do
  let PV1.Scripts.ScriptHash pScriptHashBytes = pScriptHash
  lHash <-
    pScriptHashBytes
      & Plutus.fromBuiltin
      & Cardano.Crypto.hashFromBytes
  pure $
    lHash
      & LG.ScriptHash
      & LG.ScriptHashObj

-- This is the inverse of Alonzo.TxInfo.transStakeReference,
-- except when there are deserialization errors from Plutus.
transStakeReferenceInverse ::
  Maybe PV1.Credential.StakingCredential ->
  Maybe (LG.StakeReference StandardCrypto)
transStakeReferenceInverse =
  maybe (Just LG.StakeRefNull) transStakeReferenceInverse'

-- This helper function isolates the errors that can occur
-- when deserializing a Plutus.StakingCredential from Plutus.
transStakeReferenceInverse' ::
  PV1.Credential.StakingCredential ->
  Maybe (LG.StakeReference StandardCrypto)
transStakeReferenceInverse' (PV1.Credential.StakingHash credential) = do
  lCredential <- transCredInverse credential
  pure $ LG.StakeRefBase lCredential
--
transStakeReferenceInverse' (PV1.Credential.StakingPtr pSlot pTxIx pCertIx) = do
  lTxIx <- LG.txIxFromIntegral @Integer (fromInteger pTxIx)
  lCertIx <- LG.certIxFromIntegral @Integer (fromInteger pCertIx)
  let lSlot = fromIntegral pSlot
  pure $ LG.StakeRefPtr $ LG.Ptr (LG.SlotNo lSlot) lTxIx lCertIx
