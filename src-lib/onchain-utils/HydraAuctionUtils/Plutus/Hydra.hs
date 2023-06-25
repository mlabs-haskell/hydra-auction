module HydraAuctionUtils.Plutus.Hydra (checkIsMoveToHydra) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports

import PlutusLedgerApi.V1.Value (CurrencySymbol)
import PlutusLedgerApi.V2.Contexts (ScriptContext (..), TxInfo (..))

-- Hydra imports
import Hydra.Contract.Head (hasPT)

-- HydraAuction imports
import HydraAuctionUtils.Plutus (nothingForged)
import HydraAuctionUtils.Plutus.TxOut (isNotAdaOnlyOutput)

{-# INLINEABLE checkIsMoveToHydra #-}
checkIsMoveToHydra :: ScriptContext -> CurrencySymbol -> Bool
checkIsMoveToHydra context headId =
  case filter isNotAdaOnlyOutput outputs of
    [hydraCommitOutput] ->
      -- Assets cannot be stolen, cuz we have only one output
      -- Hydra validator shoud check that datum is not changed
      -- in commited output
      -- Also validator checks that output address is correct
      -- The only thing we should check is Tx has right PT
      traceIfFalse "No Hydra Participation Token" $
        hasPT headId hydraCommitOutput
    _ -> traceError "Wrong number of Hydra move outputs"
    && nothingForged info
  where
    info = scriptContextTxInfo context
    outputs = txInfoOutputs info
