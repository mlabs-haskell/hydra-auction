module HydraAuction.Onchain.Validators.FeeEscrow (
  validator,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V2 (
  PubKeyHash,
  ScriptContext (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
 )
import PlutusLedgerApi.V2.Contexts (
  findOwnInput,
  valuePaidTo,
 )

import HydraAuction.Error.Onchain.Validators.FeeEscrow (
  FeeEscrow'Error (..),
 )
import HydraAuction.Onchain.Lib.Error (eCode, err, errMaybe)
import HydraAuction.Onchain.Lib.PlutusTx (
  lovelaceValueOf,
  onlyOneInputFromAddress,
 )
import HydraAuction.Onchain.Types.AuctionTerms (
  AuctionTerms (..),
 )
import HydraAuction.Onchain.Types.Redeemers (
  FeeEscrow'Redeemer (..),
 )

validator ::
  AuctionTerms ->
  FeeEscrow'Redeemer ->
  ScriptContext ->
  Bool
validator aTerms@AuctionTerms {..} DistributeFees context =
  ownInputIsOnlyInputFromOwnScript
    && noTokensMintedOrBurned
    && allDelegatesReceivedSufficientAda
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    --
    -- There should only be one input from the fee escrow validator.
    ownInputIsOnlyInputFromOwnScript =
      onlyOneInputFromAddress ownAddress txInfoInputs
        `err` $(eCode FeeEscrow'Error'TooManyOwnScriptInputs)
    --
    -- No tokens are minted or burned.
    noTokensMintedOrBurned =
      (txInfoMint == mempty)
        `err` $(eCode FeeEscrow'Error'UnexpectedMintOrBurn)
    --
    -- Each delegate receives at least at'AuctionFeePerDelegate ADA.
    allDelegatesReceivedSufficientAda =
      all (delegateReceivedSufficientAda aTerms txInfo) at'Delegates
        `err` $(eCode FeeEscrow'Error'InsufficientDelegateFeePayments)
    --
    -- The validator's own input should exist.
    -- Note that this should always hold for a validator being executed
    -- with a Spending script purpose.
    ownInput =
      txInInfoResolved $
        findOwnInput context
          `errMaybe` $(eCode FeeEscrow'Error'MissingOwnInput)
    ownAddress = txOutAddress ownInput
--
{-# INLINEABLE validator #-}

delegateReceivedSufficientAda :: AuctionTerms -> TxInfo -> PubKeyHash -> Bool
delegateReceivedSufficientAda AuctionTerms {..} txInfo d =
  lovelaceValueOf (valuePaidTo txInfo d) > at'AuctionFeePerDelegate
--
{-# INLINEABLE delegateReceivedSufficientAda #-}
