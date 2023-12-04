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
    ownInputIsOnlyInputFromOwnScript =
      (length ownScriptInputs == 1)
        `err` $(eCode AuctionMetadata'Error'TooManyOwnScriptInputs)
    --
    ownInputHoldsAuctionMetadataToken =
      (valueOf ownValue ai'AuctionId auctionMetadataTN == 1)
        `err` ""
    --
    auctionTokensAreBurnedExactly =
      (txInfoMint == expectedMint)
        `err` $(eCode AuctionMetadata'Error'AuctionTokensNotBurnedExactly)
    --
    ownInputTxOut =
      txInInfoResolved $
        findOwnInput context
          `errMaybe` $(eCode AuctionMetadata'Error'MissingOwnInput)
    ownAddress = txOutAddress ownInputTxOut
    ownValue = txOutValue ownInputTxOut
    ownScriptInputs =
      find
        (\x -> ownAddress == txOutAddress (txInInfoResolved x))
        txInfoInputs
    --
    AuctionInfo {ai'AuctionId} =
      case txOutDatum ownInputTxOut of
        OutputDatum d ->
          PlutusTx.fromBuiltinData (getDatum d)
            `errMaybe` $(eCode AuctionMetadata'Error'FailedToDecodeOwnDatum)
        _otherwise ->
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
