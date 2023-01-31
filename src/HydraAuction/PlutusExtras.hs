-- | More Extras for Plutus.Extras
module HydraAuction.PlutusExtras (module X, validatorAddress, scriptCurrencySymbol) where

import Hydra.Prelude hiding (fromMaybe)

import Hydra.Cardano.Api (
    PlutusScriptV2,
    SerialiseAsRawBytes (serialiseToRawBytes),
    fromPlutusScript,
    hashScript,
    pattern PlutusScript,
 )
import Plutus.Extras as X
import Plutus.V1.Ledger.Address (Address, scriptHashAddress)
import Plutus.V1.Ledger.Scripts (MintingPolicy, unMintingPolicyScript, unValidatorScript)
import Plutus.V1.Ledger.Value (CurrencySymbol (..))
import Plutus.V2.Ledger.Api (Validator)
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
