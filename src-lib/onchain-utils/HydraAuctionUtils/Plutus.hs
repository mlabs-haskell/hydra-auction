module HydraAuctionUtils.Plutus (
  nothingForged,
  module X,
) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports
import PlutusLedgerApi.V1.Value (isZero)
import PlutusLedgerApi.V2.Contexts (TxInfo (..))

-- HydraAuction imports
import HydraAuctionUtils.Plutus.Interval as X
import HydraAuctionUtils.Plutus.TxOut as X

-- Other TxInfo checks

{-# INLINEABLE nothingForged #-}
nothingForged :: TxInfo -> Bool
nothingForged info = traceIfFalse "Something was forged" (isZero $ txInfoMint info)
