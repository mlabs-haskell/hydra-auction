module HydraAuction.Onchain.Lib.ScriptContext (
  scriptOutputsAt,
  valueLockedBy,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Address (
  Address (..),
 )
import PlutusLedgerApi.V1.Credential (
  Credential (..),
 )
import PlutusLedgerApi.V1.Scripts (
  ScriptHash,
 )
import PlutusLedgerApi.V1.Value (
  Value (..),
 )
import PlutusLedgerApi.V2.Contexts (
  TxInfo (..),
 )
import PlutusLedgerApi.V2.Tx (
  OutputDatum (..),
  TxOut (..),
 )

-- * Utilities

-- Copied from hydra-plutus Hydra.ScriptContext

-- | Get the list of 'TxOut' outputs of the pending transaction at
-- a given script address.
scriptOutputsAt :: ScriptHash -> TxInfo -> [(OutputDatum, Value)]
scriptOutputsAt h p =
  let flt TxOut {txOutDatum = d, txOutAddress = Address (ScriptCredential s) _, txOutValue} | s == h = Just (d, txOutValue)
      flt _ = Nothing
   in mapMaybe flt (txInfoOutputs p)
{-# INLINEABLE scriptOutputsAt #-}

-- | Get the total value locked by the given validator in this transaction.
valueLockedBy :: TxInfo -> ScriptHash -> Value
valueLockedBy ptx h =
  let outputs = map snd (scriptOutputsAt h ptx)
   in mconcat outputs
{-# INLINEABLE valueLockedBy #-}
