module HydraAuction.Onchain.Validators.FeeEscrow (
  validator,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Value (
  adaSymbol,
  adaToken,
  valueOf,
 )
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (..),
  TxInInfo (..),
  TxInfo (..),
  findOwnInput,
  valuePaidTo,
 )
import PlutusLedgerApi.V2.Tx (
  TxOut (..),
 )

import HydraAuction.Error.Onchain.Validators.FeeEscrow (
  FeeEscrow'Error (..),
 )
import HydraAuction.Onchain.Lib.Error (eCode, err, errMaybe)
import HydraAuction.Onchain.Types.AuctionTerms (AuctionTerms (..))
import HydraAuction.Onchain.Types.Scripts (FeeEscrow'Redeemer (..))

validator ::
  AuctionTerms ->
  FeeEscrow'Redeemer ->
  ScriptContext ->
  Bool
validator AuctionTerms {..} DistributeFees context =
  ownInputIsOnlyInputFromOwnScript
    && noTokensMintedOrBurned
    && allDelegatesReceivedSufficientAda
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    --
    -- The validator's own input should exist.
    -- Note that this should always hold for a validator being executed
    -- with a Spending script purpose.
    ownInputTxOut =
      txInInfoResolved $
        findOwnInput context
          `errMaybe` $(eCode FeeEscrow'Error'MissingOwnInput)
    ownAddress = txOutAddress ownInputTxOut
    --
    -- There should only be one input from the fee escrow validator.
    ownInputIsOnlyInputFromOwnScript =
      (length ownScriptInputs == 1)
        `err` $(eCode FeeEscrow'Error'TooManyOwnScriptInputs)
    ownScriptInputs =
      filter
        (\x -> ownAddress == txOutAddress (txInInfoResolved x))
        txInfoInputs
    --
    -- No tokens are minted or burned.
    noTokensMintedOrBurned =
      (txInfoMint == mempty)
        `err` $(eCode FeeEscrow'Error'UnexpectedMintOrBurn)
    --
    -- Each delegate receives at least at'AuctionFeePerDelegate ADA.
    allDelegatesReceivedSufficientAda =
      all delegateReceivedSufficientAda at'Delegates
        `err` $(eCode FeeEscrow'Error'InsufficientDelegateFeePayments)
    delegateReceivedSufficientAda d =
      lovelaceValueOf (valuePaidTo txInfo d) > at'AuctionFeePerDelegate
    lovelaceValueOf v =
      valueOf v adaSymbol adaToken
--
{-# INLINEABLE validator #-}
