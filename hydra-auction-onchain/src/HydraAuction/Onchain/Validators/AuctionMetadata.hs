module HydraAuction.Onchain.Validators.AuctionMetadata (
  validator,
  compiledValidator,
  validatorScript,
  validatorHash,
  datum,
  redeemer,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1.Scripts (
  Datum (..),
  Redeemer (..),
  ScriptHash,
 )
import PlutusLedgerApi.V1.Value (
  Value (..),
  valueOf,
 )
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (..),
  TxInInfo (..),
  TxInfo (..),
  findOwnInput,
 )
import PlutusLedgerApi.V2.Tx (
  OutputDatum (..),
  TxOut (..),
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

import HydraAuction.Error.Onchain.Validators.AuctionMetadata (
  AuctionMetadata'Error (..),
 )
import HydraAuction.Onchain.Lib.Error (eCode, err, errMaybe)
import HydraAuction.Onchain.Lib.PlutusScript (
  ValidatorType,
  scriptValidatorHash',
  wrapValidator,
 )
import HydraAuction.Onchain.Types.AuctionInfo (
  AuctionInfo (..),
  auctionMetadataTN,
  auctionTN,
  standingBidTN,
 )

data AuctionMetadata'Redeemer
  = RemoveAuction

PlutusTx.unstableMakeIsData ''AuctionMetadata'Redeemer

type DatumType = ()
type RedeemerType = AuctionMetadata'Redeemer

-- -------------------------------------------------------------------------
-- Validator
-- -------------------------------------------------------------------------
validator ::
  DatumType ->
  RedeemerType ->
  ScriptContext ->
  Bool
validator () RemoveAuction context =
  ownInputIsOnlyInputFromOwnScript
    && ownInputHoldsAuctionMetadataToken
    && auctionTokensAreBurnedExactly
  where
    TxInfo {..} = scriptContextTxInfo context
    --
    -- (AuctionMetadata01)
    -- There should only be one input from the auction metadata validator.
    ownInputIsOnlyInputFromOwnScript =
      (length ownScriptInputs == 1)
        `err` $(eCode AuctionMetadata'Error'TooManyOwnScriptInputs)
    --
    -- (AuctionMetadata02)
    -- The validator's own input should hold one auction metadata token
    -- with a currency symbol matching the auction ID
    -- mentioned in the auction info metadata record.
    ownInputHoldsAuctionMetadataToken =
      (valueOf ownValue ai'AuctionId auctionMetadataTN == 1)
        `err` $(eCode AuctionMetadata'Error'OwnInputMissingMetadataToken)
    --
    -- (AuctionMetadata03)
    -- The auction state, auction metadata, and standing bid tokens
    -- of the auction should all be burned.
    -- No other tokens should be minted or burned.
    auctionTokensAreBurnedExactly =
      (txInfoMint == expectedMint)
        `err` $(eCode AuctionMetadata'Error'AuctionTokensNotBurnedExactly)
    --
    -- (AuctionMetadata04)
    -- The validator's own input should exist.
    -- Note that this should always hold for a validator being executed
    -- with a Spending script purpose.
    ownInputTxOut =
      txInInfoResolved $
        findOwnInput context
          `errMaybe` $(eCode AuctionMetadata'Error'MissingOwnInput)
    ownAddress = txOutAddress ownInputTxOut
    ownValue = txOutValue ownInputTxOut
    ownScriptInputs =
      filter
        (\x -> ownAddress == txOutAddress (txInInfoResolved x))
        txInfoInputs
    --
    AuctionInfo {ai'AuctionId} =
      case txOutDatum ownInputTxOut of
        OutputDatum d ->
          --
          -- (AuctionMetadata05)
          -- The validator's own input's datum should be decodable
          -- as an auction info metadta record.
          PlutusTx.fromBuiltinData (getDatum d)
            `errMaybe` $(eCode AuctionMetadata'Error'FailedToDecodeOwnDatum)
        _otherwise ->
          --
          -- (AuctionMetadata06)
          -- The validator's own input should contain an inline datum.
          traceError $(eCode AuctionMetadata'Error'MissingOwnDatum)
    --
    expectedMint =
      Value $
        AssocMap.singleton ai'AuctionId $
          AssocMap.fromList
            [ (auctionTN, -1)
            , (auctionMetadataTN, -1)
            , (standingBidTN, -1)
            ]

-- -------------------------------------------------------------------------
-- Script compilation
-- -------------------------------------------------------------------------
compiledValidator :: PlutusTx.CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap validator||])
  where
    wrap = wrapValidator @DatumType @RedeemerType

validatorScript :: SerialisedScript
validatorScript = serialiseCompiledCode compiledValidator

validatorHash :: ScriptHash
validatorHash = scriptValidatorHash' validatorScript

datum :: DatumType -> Datum
datum a = Datum (PlutusTx.toBuiltinData a)

redeemer :: RedeemerType -> Redeemer
redeemer a = Redeemer (PlutusTx.toBuiltinData a)
