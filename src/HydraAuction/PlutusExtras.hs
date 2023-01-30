-- | More Extras for Plutus.Extras
module HydraAuction.PlutusExtras (module X, validatorAddress, scriptCurrencySymbol) where

import Hydra.Prelude hiding (fromMaybe)

import Hydra.Cardano.Api (
  SerialiseAsRawBytes (serialiseToRawBytes),
  fromPlutusScript,
  hashScript,
  pattern PlutusScript,
  PlutusScriptV2
 )
import Plutus.Extras as X
import Plutus.V1.Ledger.Address (Address, scriptHashAddress)
import Plutus.V1.Ledger.Api (Validator )
import Plutus.V1.Ledger.Scripts (MintingPolicy, unMintingPolicyScript, unValidatorScript)
import Plutus.V1.Ledger.Value (CurrencySymbol (..))
import PlutusTx.Prelude (toBuiltin)

validatorAddress :: Validator -> Address
validatorAddress = scriptHashAddress . scriptValidatorHash . unValidatorScript

{-# INLINEABLE scriptCurrencySymbol #-}
scriptCurrencySymbol :: MintingPolicy -> CurrencySymbol
scriptCurrencySymbol =
  CurrencySymbol . toBuiltin
    . serialiseToRawBytes
    . hashScript
    . PlutusScript
    . fromPlutusScript
    . unMintingPolicyScript

