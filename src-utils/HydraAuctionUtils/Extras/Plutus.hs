-- | More Extras for Plutus.Extras
module HydraAuctionUtils.Extras.Plutus (module X, validatorAddress, scriptCurrencySymbol) where

-- Prelude imports

import PlutusTx.Prelude (toBuiltin)
import Prelude

-- Plutus imports
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V1.Address (Address, scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))

-- Plutus extra imports
import Plutus.Extras as X

-- Hydra imports
import Hydra.Cardano.Api (
  SerialiseAsRawBytes (serialiseToRawBytes),
  fromPlutusScript,
  hashScript,
  pattern PlutusScript,
 )

validatorAddress :: SerialisedScript -> Address
validatorAddress = scriptHashAddress . scriptValidatorHash

{-# INLINEABLE scriptCurrencySymbol #-}
scriptCurrencySymbol :: SerialisedScript -> CurrencySymbol
scriptCurrencySymbol =
  CurrencySymbol
    . toBuiltin
    . serialiseToRawBytes
    . hashScript
    . PlutusScript
    . fromPlutusScript
