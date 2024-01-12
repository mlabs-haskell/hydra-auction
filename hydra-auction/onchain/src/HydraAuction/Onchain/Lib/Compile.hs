module HydraAuction.Onchain.Lib.Compile (
  ValidatorType,
  MintingPolicyType,
  detypeStatelessValidator,
  detypeValidator,
  detypeMintingPolicy,
) where

import Prelude

import PlutusTx (BuiltinData, UnsafeFromData (..))
import PlutusTx.Prelude (check)

-- * Vendored from hydra-plutus-extras (who vendored it from plutus-ledger)

-- | Signature of an untyped validator script.
type ValidatorType = BuiltinData -> BuiltinData -> BuiltinData -> ()

-- | Erase a typed validator's types to get the untyped
-- `ValidatorType` signature which can be passed to `PlutusTx.compile`.
detypeValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer, UnsafeFromData context) =>
  (datum -> redeemer -> context -> Bool) ->
  ValidatorType
detypeValidator f d r c =
  check $ f datum redeemer context
  where
    datum = unsafeFromBuiltinData d
    redeemer = unsafeFromBuiltinData r
    context = unsafeFromBuiltinData c
--
{-# INLINEABLE detypeValidator #-}

-- | Untype a typed stateless validator to get the untyped
-- `ValidatorType` signature which can be passed to
-- `PlutusTx.compile`.
detypeStatelessValidator ::
  (UnsafeFromData redeemer, UnsafeFromData context) =>
  (redeemer -> context -> Bool) ->
  ValidatorType
detypeStatelessValidator f _d r c =
  check $ f redeemer context
  where
    redeemer = unsafeFromBuiltinData r
    context = unsafeFromBuiltinData c
--
{-# INLINEABLE detypeStatelessValidator #-}

-- | Signature of an untyped minting policy script.
type MintingPolicyType = BuiltinData -> BuiltinData -> ()

-- | Untype a typed minting policy to get the untyped
-- `MintingPolicyType` signature which can be passed to `PlutusTx.compile`.
detypeMintingPolicy ::
  (UnsafeFromData redeemer, UnsafeFromData context) =>
  (redeemer -> context -> Bool) ->
  MintingPolicyType
detypeMintingPolicy f r c =
  check $ f redeemer context
  where
    redeemer = unsafeFromBuiltinData r
    context = unsafeFromBuiltinData c
--
{-# INLINEABLE detypeMintingPolicy #-}
