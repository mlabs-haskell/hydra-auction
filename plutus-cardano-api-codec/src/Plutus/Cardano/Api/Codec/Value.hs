module Plutus.Cardano.Api.Codec.Value (
  -- Offchain -> Onchain
  toPlutusAssetId,
  toPlutusAssetName,
  toPlutusLovelace,
  toPlutusPolicyId,
  toPlutusValue,
  -- Onchain -> Offchain
  fromPlutusAssetId,
  fromPlutusAssetName,
  fromPlutusLovelace,
  fromPlutusPolicyId,
  fromPlutusValue,
) where

import Prelude

import Data.Function ((&))

import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as Short

import Cardano.Api.Shelley qualified as CA

import Cardano.Crypto.Hash qualified as Cardano.Crypto

import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo.TxInfo
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Hashes qualified as Ledger
import Cardano.Ledger.Mary.Value qualified as Mary

import PlutusLedgerApi.V1.Value qualified as PV1.Value

import PlutusTx.Prelude qualified as Plutus

-- -------------------------------------------------------------------------
-- Value
-- -------------------------------------------------------------------------
toPlutusValue :: CA.Value -> PV1.Value.Value
toPlutusValue cValue =
  cValue
    & CA.valueToList
    & foldMap cardanoValueTupleToPlutusSingletonValue

fromPlutusValue :: PV1.Value.Value -> Maybe CA.Value
fromPlutusValue pValue = do
  cValueList <-
    pValue
      & PV1.Value.flattenValue
      & traverse plutusValueTupleToCardanoValueTuple
  pure $ CA.valueFromList cValueList

cardanoValueTupleToPlutusSingletonValue ::
  (CA.AssetId, CA.Quantity) ->
  PV1.Value.Value
cardanoValueTupleToPlutusSingletonValue (cAssetId, CA.Quantity n) =
  PV1.Value.assetClassValue (toPlutusAssetId cAssetId) n

plutusValueTupleToCardanoValueTuple ::
  (PV1.Value.CurrencySymbol, PV1.Value.TokenName, Integer) ->
  Maybe (CA.AssetId, CA.Quantity)
plutusValueTupleToCardanoValueTuple (pPolicyId, pAssetName, n) = do
  cAssetId <-
    PV1.Value.assetClass pPolicyId pAssetName
      & fromPlutusAssetId
  pure (cAssetId, CA.Quantity n)

-- -------------------------------------------------------------------------
-- Asset ID
-- -------------------------------------------------------------------------
toPlutusAssetId :: CA.AssetId -> PV1.Value.AssetClass
toPlutusAssetId CA.AdaAssetId =
  PV1.Value.AssetClass (PV1.Value.adaSymbol, PV1.Value.adaToken)
toPlutusAssetId (CA.AssetId cPolicyId cAssetName) =
  let
    pPolicyId = toPlutusPolicyId cPolicyId
    pAssetName = toPlutusAssetName cAssetName
   in
    PV1.Value.AssetClass (pPolicyId, pAssetName)

fromPlutusAssetId :: PV1.Value.AssetClass -> Maybe CA.AssetId
fromPlutusAssetId (PV1.Value.AssetClass (pPolicyId, pAssetName)) =
  if pPolicyId == PV1.Value.adaSymbol && pAssetName == PV1.Value.adaToken
    then pure CA.AdaAssetId
    else do
      cPolicyId <- fromPlutusPolicyId pPolicyId
      let cAssetName = fromPlutusAssetName pAssetName
      pure $ CA.AssetId cPolicyId cAssetName

-- -------------------------------------------------------------------------
-- Policy ID
-- -------------------------------------------------------------------------
toPlutusPolicyId :: CA.PolicyId -> PV1.Value.CurrencySymbol
toPlutusPolicyId (CA.PolicyId cPolicyHash) =
  cPolicyHash
    & CA.toShelleyScriptHash
    & Mary.PolicyID
    & Alonzo.TxInfo.transPolicyID

fromPlutusPolicyId :: PV1.Value.CurrencySymbol -> Maybe CA.PolicyId
fromPlutusPolicyId pPolicyId = do
  Mary.PolicyID lPolicyHash <-
    transPolicyIDInverse pPolicyId
  pure $
    lPolicyHash
      & CA.fromShelleyScriptHash
      & CA.PolicyId

-- This is the inverse of Alonzo.TxInfo.transPolicyID,
-- except when the Plutus.CurrencySymbol bytestring cannot be
-- decoded as a valid minting policy script hash.
transPolicyIDInverse ::
  PV1.Value.CurrencySymbol ->
  Maybe (Mary.PolicyID StandardCrypto)
transPolicyIDInverse (PV1.Value.CurrencySymbol pPolicyHash) = do
  hash <-
    pPolicyHash
      & Plutus.fromBuiltin
      & Cardano.Crypto.hashFromBytes
  pure $
    hash
      & Ledger.ScriptHash
      & Mary.PolicyID

-- -------------------------------------------------------------------------
-- Asset name
-- -------------------------------------------------------------------------
toPlutusAssetName :: CA.AssetName -> PV1.Value.TokenName
toPlutusAssetName (CA.AssetName cAssetName) =
  cAssetName
    & toMaryAssetName
    & Alonzo.TxInfo.transAssetName

fromPlutusAssetName :: PV1.Value.TokenName -> CA.AssetName
fromPlutusAssetName pAssetName =
  pAssetName
    & transAssetNameInverse
    & fromMaryAssetName
    & CA.AssetName

transAssetNameInverse :: PV1.Value.TokenName -> Mary.AssetName
transAssetNameInverse (PV1.Value.TokenName pAssetNameBytes) =
  pAssetNameBytes
    & Plutus.fromBuiltin
    & toMaryAssetName

toMaryAssetName :: BS.ByteString -> Mary.AssetName
toMaryAssetName pAssetNameBytes =
  pAssetNameBytes
    & Short.toShort
    & Mary.AssetName

fromMaryAssetName :: Mary.AssetName -> BS.ByteString
fromMaryAssetName mAssetName =
  mAssetName
    & Mary.assetName
    & Short.fromShort

-- -------------------------------------------------------------------------
-- Lovelace integers
-- -------------------------------------------------------------------------
toPlutusLovelace :: CA.Lovelace -> Integer
toPlutusLovelace (CA.Lovelace n) = n

fromPlutusLovelace :: Integer -> CA.Lovelace
fromPlutusLovelace = CA.Lovelace
