module HydraAuction.Onchain.Lib.PlutusTx (
  lovelaceValueOf,
  onlyOneInputFromAddress,
  parseInlineDatum,
  --
  findOwnInputWithStateToken,
  findTxOutWithStateTokenAtAddr,
  findTxOutWithStateTokenAtSh,
  --
  txOutHasStateTokenAtAddr,
  txOutHasStateTokenAtSh,
  txOutHasStateToken,
  txOutIsAtAddr,
  txOutIsAtSh,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Address (
  Address,
  toScriptHash,
 )
import PlutusLedgerApi.V1.Scripts (
  Datum (..),
  ScriptHash,
 )
import PlutusLedgerApi.V1.Value (
  CurrencySymbol,
  TokenName,
  Value,
  adaSymbol,
  adaToken,
  valueOf,
 )
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (..),
  TxInInfo (..),
  findOwnInput,
 )
import PlutusLedgerApi.V2.Tx (
  OutputDatum (..),
  TxOut (..),
 )
import PlutusTx qualified

-- -------------------------------------------------------------------------
-- Basic utilities
-- -------------------------------------------------------------------------

-- | There's only one input at the given address.
onlyOneInputFromAddress ::
  Address ->
  [TxInInfo] ->
  Bool
onlyOneInputFromAddress address inputs = length inputs' == 1
  where
    inputs' =
      filter (\x -> address == txOutAddress (txInInfoResolved x)) inputs
--
{-# INLINEABLE onlyOneInputFromAddress #-}

-- | Try to parse a tx output datum, if it is inline.
parseInlineDatum ::
  PlutusTx.FromData a =>
  TxOut ->
  Maybe a
parseInlineDatum TxOut {txOutDatum = OutputDatum (Datum d)} =
  PlutusTx.fromBuiltinData d
parseInlineDatum _ = Nothing
--
{-# INLINEABLE parseInlineDatum #-}

lovelaceValueOf :: Value -> Integer
lovelaceValueOf v =
  valueOf v adaSymbol adaToken

-- -------------------------------------------------------------------------
-- State token utilities
-- -------------------------------------------------------------------------

-- | Find the input currently being validated if
-- it contains the given state token.
findOwnInputWithStateToken ::
  CurrencySymbol ->
  TokenName ->
  ScriptContext ->
  Maybe TxInInfo
findOwnInputWithStateToken cs tn context = do
  ownInput <- findOwnInput context
  let hasStateToken = txOutHasStateToken cs tn (txInInfoResolved ownInput)
  if hasStateToken then Just ownInput else Nothing
--
{-# INLINEABLE findOwnInputWithStateToken #-}

-- | Find the tx output located at a given address
-- and containing the given state token.
findTxOutWithStateTokenAtAddr ::
  CurrencySymbol ->
  TokenName ->
  Address ->
  [TxOut] ->
  Maybe TxOut
findTxOutWithStateTokenAtAddr cs tn addr =
  find (txOutHasStateTokenAtAddr cs tn addr)
--
{-# INLINEABLE findTxOutWithStateTokenAtAddr #-}

-- | Find the tx output located at a script address
-- corresponding to a given script hash
-- and containing a given state token.
-- WARNING: This function assumes that the state token is unique.
findTxOutWithStateTokenAtSh ::
  CurrencySymbol ->
  TokenName ->
  ScriptHash ->
  [TxOut] ->
  Maybe TxOut
findTxOutWithStateTokenAtSh cs tn sh = do
  find (txOutHasStateTokenAtSh cs tn sh)
--
{-# INLINEABLE findTxOutWithStateTokenAtSh #-}

txOutHasStateTokenAtAddr ::
  CurrencySymbol ->
  TokenName ->
  Address ->
  TxOut ->
  Bool
txOutHasStateTokenAtAddr cs tn addr x =
  txOutIsAtAddr addr x && txOutHasStateToken cs tn x
--
{-# INLINEABLE txOutHasStateTokenAtAddr #-}

-- | Is the tx output located at the given script address
-- and does it contain the given state token?
txOutHasStateTokenAtSh ::
  CurrencySymbol ->
  TokenName ->
  ScriptHash ->
  TxOut ->
  Bool
txOutHasStateTokenAtSh cs tn sh x =
  txOutIsAtSh sh x && txOutHasStateToken cs tn x
--
{-# INLINEABLE txOutHasStateTokenAtSh #-}

txOutHasStateToken :: CurrencySymbol -> TokenName -> TxOut -> Bool
txOutHasStateToken cs tn TxOut {..} =
  valueOf txOutValue cs tn == 1
--
{-# INLINEABLE txOutHasStateToken #-}

txOutIsAtAddr :: Address -> TxOut -> Bool
txOutIsAtAddr addr TxOut {..} = addr == txOutAddress
--
{-# INLINEABLE txOutIsAtAddr #-}

txOutIsAtSh :: ScriptHash -> TxOut -> Bool
txOutIsAtSh sh TxOut {..} =
  Just sh == toScriptHash txOutAddress
--
{-# INLINEABLE txOutIsAtSh #-}
