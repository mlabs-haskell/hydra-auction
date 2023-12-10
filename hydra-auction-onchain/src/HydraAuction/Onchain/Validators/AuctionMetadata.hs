module HydraAuction.Onchain.Validators.AuctionMetadata (
  validator,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V2.Contexts (
  ScriptContext (..),
  TxInInfo (..),
  TxInfo (..),
 )
import PlutusLedgerApi.V2.Tx (
  TxOut (..),
 )

import HydraAuction.Error.Onchain.Validators.AuctionMetadata (
  AuctionMetadata'Error (..),
 )
import HydraAuction.Onchain.Lib.Error (eCode, err, errMaybe)
import HydraAuction.Onchain.Lib.PlutusTx (
  onlyOneInputFromAddress,
 )
import HydraAuction.Onchain.Types.AuctionInfo (
  AuctionInfo (..),
 )
import HydraAuction.Onchain.Types.Scripts (
  AuctionID (..),
  AuctionMetadata'Redeemer (..),
  allAuctionTokensBurned,
  findAuctionMetadataOwnInput,
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
    && auctionTokensAreBurnedExactly
  where
    TxInfo {..} = scriptContextTxInfo context
    auctionID = AuctionID ai'AuctionId
    --
    -- There should only be one input from the auction metadata validator.
    ownInputIsOnlyInputFromOwnScript =
      onlyOneInputFromAddress ownAddress txInfoInputs
        `err` $(eCode AuctionMetadata'Error'TooManyOwnScriptInputs)
    --
    -- The auction state, auction metadata, and standing bid tokens
    -- of the auction should all be burned.
    -- No other tokens should be minted or burned.
    auctionTokensAreBurnedExactly =
      (txInfoMint == allAuctionTokensBurned auctionID)
        `err` $(eCode AuctionMetadata'Error'AuctionTokensNotBurnedExactly)
    --
    -- The validator's own input should exist and
    -- it should contain an auction metadata token.
    ownInput =
      txInInfoResolved $
        findAuctionMetadataOwnInput auctionID context
          `errMaybe` $(eCode AuctionMetadata'Error'MissingMetadataInput)
    ownAddress = txOutAddress ownInput
--
{-# INLINEABLE validator #-}
