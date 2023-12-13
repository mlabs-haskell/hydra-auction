module Lib.Scripts (
  StatelessValidatorType,
  ValidatorType,
  MintingPolicyType,
  wrapStatelessValidator,
  wrapValidator,
  wrapMintingPolicy,
) where

import Prelude

import PlutusTx (BuiltinData, UnsafeFromData (..))
import PlutusTx.Prelude (check)

-- * Vendored from hydra-plutus-extras (who vendored it from plutus-ledger)

-- | Signature of an untyped validator script.
type ValidatorType = BuiltinData -> BuiltinData -> BuiltinData -> ()

-- | Wrap a typed validator to get the basic
-- `ValidatorType` signature which can be passed to `PlutusTx.compile`.
wrapValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer, UnsafeFromData context) =>
  (datum -> redeemer -> context -> Bool) ->
  ValidatorType
wrapValidator f d r c =
  check $ f datum redeemer context
  where
    datum = unsafeFromBuiltinData d
    redeemer = unsafeFromBuiltinData r
    context = unsafeFromBuiltinData c
--
{-# INLINEABLE wrapValidator #-}

-- | Signature of an untyped validator script without a datum.
-- This the same as the untyped minting policy signature,
-- but separated just in case.
type StatelessValidatorType = BuiltinData -> BuiltinData -> ()

-- | Wrap a typed stateless validator to get the basic
-- `StatelessValidatorType` signature which can be passed to
-- `PlutusTx.compile`.
wrapStatelessValidator ::
  (UnsafeFromData redeemer, UnsafeFromData context) =>
  (redeemer -> context -> Bool) ->
  StatelessValidatorType
wrapStatelessValidator f r c =
  check $ f redeemer context
  where
    redeemer = unsafeFromBuiltinData r
    context = unsafeFromBuiltinData c
--
{-# INLINEABLE wrapStatelessValidator #-}

-- | Signature of an untyped minting policy script.
type MintingPolicyType = BuiltinData -> BuiltinData -> ()

-- | Wrap a typed minting policy to get the basic
-- `MintingPolicyType` signature which can be passed to `PlutusTx.compile`.
wrapMintingPolicy ::
  (UnsafeFromData redeemer, UnsafeFromData context) =>
  (redeemer -> context -> Bool) ->
  MintingPolicyType
wrapMintingPolicy f r c =
  check $ f redeemer context
  where
    redeemer = unsafeFromBuiltinData r
    context = unsafeFromBuiltinData c
--
{-# INLINEABLE wrapMintingPolicy #-}
