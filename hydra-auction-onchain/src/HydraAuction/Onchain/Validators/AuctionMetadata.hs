module HydraAuction.Onchain.Validators.AuctionMetadata (
  validator,
) where

import PlutusTx.Prelude

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
  TxOut (..),
 )
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
import HydraAuction.Onchain.Types.Scripts (
  AuctionMetadata'Redeemer (..),
 )

-- -------------------------------------------------------------------------
-- Validator
-- -------------------------------------------------------------------------
validator ::
  AuctionInfo ->
  AuctionMetadata'Redeemer ->
  ScriptContext ->
  Bool
validator AuctionInfo {..} RemoveAuction context =
  ownInputIsOnlyInputFromOwnScript
    && ownInputHoldsAuctionMetadataToken
    && auctionTokensAreBurnedExactly
  where
    TxInfo {..} = scriptContextTxInfo context
    --
    -- (AuctionMetadata01)
    -- The validator's own input should exist.
    -- Note that this should always hold for a validator being executed
    -- with a Spending script purpose.
    ownInputTxOut =
      txInInfoResolved $
        findOwnInput context
          `errMaybe` $(eCode AuctionMetadata'Error'MissingOwnInput)
    ownAddress = txOutAddress ownInputTxOut
    ownValue = txOutValue ownInputTxOut
    --
    -- (AuctionMetadata02)
    -- There should only be one input from the auction metadata validator.
    ownInputIsOnlyInputFromOwnScript =
      (length ownScriptInputs == 1)
        `err` $(eCode AuctionMetadata'Error'TooManyOwnScriptInputs)
    ownScriptInputs =
      filter
        (\x -> ownAddress == txOutAddress (txInInfoResolved x))
        txInfoInputs
    --
    -- (AuctionMetadata03)
    -- The validator's own input should hold one auction metadata token
    -- with a currency symbol matching the auction ID
    -- mentioned in the auction info metadata record.
    ownInputHoldsAuctionMetadataToken =
      (valueOf ownValue ai'AuctionId auctionMetadataTN == 1)
        `err` $(eCode AuctionMetadata'Error'OwnInputMissingMetadataToken)
    --
    -- (AuctionMetadata04)
    -- The auction state, auction metadata, and standing bid tokens
    -- of the auction should all be burned.
    -- No other tokens should be minted or burned.
    auctionTokensAreBurnedExactly =
      (txInfoMint == expectedMint)
        `err` $(eCode AuctionMetadata'Error'AuctionTokensNotBurnedExactly)
    expectedMint =
      Value $
        AssocMap.singleton ai'AuctionId $
          AssocMap.fromList
            [ (auctionTN, -1)
            , (auctionMetadataTN, -1)
            , (standingBidTN, -1)
            ]
