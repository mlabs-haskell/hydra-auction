module HydraAuction.Onchain.Validators.AuctionMetadata (
  validator,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V2 (
  ScriptContext (..),
  TxInInfo (..),
  TxInfo (..),
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
import HydraAuction.Onchain.Types.Redeemers (
  AuctionMetadata'Redeemer (..),
 )
import HydraAuction.Onchain.Types.Scripts (
  findAuctionMetadataOwnInput,
 )
import HydraAuction.Onchain.Types.Tokens (
  AuctionId (..),
  allAuctionTokensBurned,
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
    auctionId = AuctionId ai'AuctionId
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
      (txInfoMint == allAuctionTokensBurned auctionId)
        `err` $(eCode AuctionMetadata'Error'AuctionTokensNotBurnedExactly)
    --
    -- The validator's own input should exist and
    -- it should contain an auction metadata token.
    ownInput =
      txInInfoResolved $
        findAuctionMetadataOwnInput auctionId context
          `errMaybe` $(eCode AuctionMetadata'Error'MissingMetadataInput)
    ownAddress = txOutAddress ownInput
--
{-# INLINEABLE validator #-}
