-- | More Extras for Plutus.Extras
module HydraAuction.Plutus.Extras (module X, validatorAddress, scriptCurrencySymbol) where

-- Prelude imports

import PlutusTx.Prelude (toBuiltin)
import Prelude

-- Plutus imports
import Plutus.V1.Ledger.Address (Address, scriptHashAddress)
import Plutus.V1.Ledger.Scripts (MintingPolicy, unMintingPolicyScript, unValidatorScript)
import Plutus.V1.Ledger.Value (CurrencySymbol (..))
import Plutus.V2.Ledger.Api (Validator)

-- Plutus extra imports
import Plutus.Extras as X

-- Hydra imports
import Hydra.Cardano.Api (
  SerialiseAsRawBytes (serialiseToRawBytes),
  fromPlutusScript,
  hashScript,
  pattern PlutusScript,
 )

validatorAddress :: Validator -> Address
validatorAddress = scriptHashAddress . scriptValidatorHash . unValidatorScript

{-# INLINEABLE scriptCurrencySymbol #-}
scriptCurrencySymbol :: MintingPolicy -> CurrencySymbol
scriptCurrencySymbol =
  CurrencySymbol
    . toBuiltin
    . serialiseToRawBytes
    . hashScript
    . PlutusScript
    . fromPlutusScript
    . unMintingPolicyScript
