module HydraAuction.Onchain.Lib.PlutusTx (
  asDatum,
  asRedeemer,
  getSpentInputRedeemer,
  lovelaceValueOf,
  onlyOneInputFromAddress,
  parseInlineDatum,
  parseRedemeer,
  scriptOutputsAtSh,
  valuePaidToScript,
  --
  findOwnInputWithStateToken,
  findInputWithStateToken,
  findInputWithStateTokenAtSh,
  findTxOutWithStateToken,
  findTxOutWithStateTokenAtAddr,
  findTxOutWithStateTokenAtSh,
  --
  txOutHasStateTokenAtAddr,
  txOutHasStateTokenAtSh,
  txOutHasStateToken,
  txOutIsAtAddr,
  txOutIsAtSh,
  txOutRefSpentWithRedeemer,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Address (
  toScriptHash,
 )
import PlutusLedgerApi.V1.Value (
  valueOf,
 )
import PlutusLedgerApi.V2 (
  Address,
  CurrencySymbol,
  Datum (..),
  OutputDatum (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash,
  ScriptPurpose (..),
  TokenName,
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  Value,
  adaSymbol,
  adaToken,
 )
import PlutusLedgerApi.V2.Contexts (
  findOwnInput,
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

-- -------------------------------------------------------------------------
-- Basic utilities
-- -------------------------------------------------------------------------

asDatum :: (PlutusTx.ToData a) => a -> Datum
asDatum = Datum . PlutusTx.toBuiltinData
--
{-# INLINEABLE asDatum #-}

asRedeemer :: (PlutusTx.ToData a) => a -> Redeemer
asRedeemer = Redeemer . PlutusTx.toBuiltinData
--
{-# INLINEABLE asRedeemer #-}

-- | Get the redeemer with which an input is being spent.
getSpentInputRedeemer :: TxInfo -> TxInInfo -> Maybe Redeemer
getSpentInputRedeemer TxInfo {..} TxInInfo {..} =
  AssocMap.lookup scriptPurpose txInfoRedeemers
  where
    scriptPurpose = Spending txInInfoOutRef
--
{-# INLINEABLE getSpentInputRedeemer #-}

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

lovelaceValueOf :: Value -> Integer
lovelaceValueOf v =
  valueOf v adaSymbol adaToken
--
{-# INLINEABLE lovelaceValueOf #-}

-- | Try to parse a tx output datum, if it is inline.
parseInlineDatum ::
  (PlutusTx.FromData a) =>
  TxOut ->
  Maybe a
parseInlineDatum TxOut {txOutDatum = OutputDatum (Datum d)} =
  PlutusTx.fromBuiltinData d
parseInlineDatum _ = Nothing
--
{-# INLINEABLE parseInlineDatum #-}

parseRedemeer ::
  (PlutusTx.FromData a) =>
  Redeemer ->
  Maybe a
parseRedemeer (Redeemer x) = PlutusTx.fromBuiltinData x
--
{-# INLINEABLE parseRedemeer #-}

scriptOutputsAtSh :: TxInfo -> ScriptHash -> [TxOut]
scriptOutputsAtSh TxInfo {..} sh =
  filter (txOutIsAtSh sh) txInfoOutputs
--
{-# INLINEABLE scriptOutputsAtSh #-}

valuePaidToScript :: TxInfo -> ScriptHash -> Value
valuePaidToScript txInfo sh =
  foldMap txOutValue $ scriptOutputsAtSh txInfo sh
--
{-# INLINEABLE valuePaidToScript #-}

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

findInputWithStateToken ::
  CurrencySymbol ->
  TokenName ->
  [TxInInfo] ->
  Maybe TxInInfo
findInputWithStateToken cs tn =
  find (txOutHasStateToken cs tn . txInInfoResolved)
--
{-# INLINEABLE findInputWithStateToken #-}

findInputWithStateTokenAtSh ::
  CurrencySymbol ->
  TokenName ->
  ScriptHash ->
  [TxInInfo] ->
  Maybe TxInInfo
findInputWithStateTokenAtSh cs tn sh =
  find (txOutHasStateTokenAtSh cs tn sh . txInInfoResolved)
--
{-# INLINEABLE findInputWithStateTokenAtSh #-}

findTxOutWithStateToken ::
  CurrencySymbol ->
  TokenName ->
  [TxOut] ->
  Maybe TxOut
findTxOutWithStateToken cs tn =
  find (txOutHasStateToken cs tn)
--
{-# INLINEABLE findTxOutWithStateToken #-}

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

-- -------------------------------------------------------------------------
-- Predicates on tx outputs
-- -------------------------------------------------------------------------

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

-- | Is the given tx output reference being spent
-- in the transaction with the given redeemer?
txOutRefSpentWithRedeemer :: TxInfo -> TxInInfo -> Redeemer -> Bool
txOutRefSpentWithRedeemer txInfo x redeemer =
  Just redeemer == getSpentInputRedeemer txInfo x
--
{-# INLINEABLE txOutRefSpentWithRedeemer #-}
