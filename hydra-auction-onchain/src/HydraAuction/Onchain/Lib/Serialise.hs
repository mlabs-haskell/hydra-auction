module HydraAuction.Onchain.Lib.Serialise (
  serialise,
) where

import PlutusTx.Prelude

import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins

serialise :: PlutusTx.ToData a => a -> BuiltinByteString
serialise = Builtins.serialiseData . PlutusTx.toBuiltinData
--
{-# INLINEABLE serialise #-}
