module HydraAuction.Onchain.Validators.AuctionMetadata (
  validator,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Scripts (
  Datum (..),
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
import HydraAuction.Onchain.Types.AuctionInfo (
  AuctionInfo (..),
  auctionMetadataTN,
  auctionTN,
  standingBidTN,
 )

data AuctionMetadata'Redeemer
  = RemoveAuction

PlutusTx.unstableMakeIsData ''AuctionMetadata'Redeemer

-- -------------------------------------------------------------------------
-- Validator
-- -------------------------------------------------------------------------
validator ::
  AuctionMetadata'Redeemer ->
  ScriptContext ->
  Bool
validator RemoveAuction context =
  exactlyOneOwnScriptInput
    && ownInputHoldsAuctionMetadataToken
    && exactlyBurnedAuctionTokens
  where
    TxInfo {..} = scriptContextTxInfo context
    --
    exactlyOneOwnScriptInput =
      (length ownScriptInputs == 1)
        `err` $(eCode AuctionMetadata'Error'TooManyOwnScriptInputs)
    --
    ownInputHoldsAuctionMetadataToken =
      (valueOf ownValue ai'AuctionId auctionMetadataTN == 1)
        `err` ""
    --
    exactlyBurnedAuctionTokens =
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
