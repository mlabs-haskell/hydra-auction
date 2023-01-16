module HydraAuction.StandingBidValidator (
  mkStandingBidValidator,
) where

import Plutus.V1.Ledger.Api (ScriptContext)
import PlutusTx.Bool (Bool (False))

import Ledger.Typed.Scripts qualified as Scripts

import HydraAuction.Types (StandingBidDatum (StandingBidDatum))

-- | The type used for the TypedValidator of the idoValidator script
data StandingBidValidatorTyped

instance Scripts.ValidatorTypes StandingBidValidatorTyped where
  type DatumType StandingBidValidatorTyped = StandingBidDatum
  type RedeemerType StandingBidValidatorTyped = ()

mkStandingBidValidator :: StandingBidDatum -> () -> ScriptContext -> Bool
mkStandingBidValidator datum redeemer ctx = False

standingBidValidatorTyped :: Scripts.TypedValidator StandingBidValidatorTyped
standingBidValidatorTyped =
  Scripts.mkTypedValidator @StandingBidValidatorTyped
    ($$ PlutusTx.compile [||mkStakeValidatorScript||])
    $$ PlutusTx.compile [||wrap||]
  where
    wrap =
      Scripts.wrapValidator
        @StandingBidDatum
        @()
