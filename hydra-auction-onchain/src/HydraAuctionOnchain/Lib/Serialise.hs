module HydraAuctionOnchain.Lib.Serialise (
  serialise,
) where

import PlutusTx.Prelude

import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins

{-# INLINEABLE serialise #-}
serialise :: PlutusTx.ToData a => a -> BuiltinByteString
serialise = Builtins.serialiseData . PlutusTx.toBuiltinData
