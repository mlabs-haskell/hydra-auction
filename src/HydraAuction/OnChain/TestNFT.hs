module HydraAuction.OnChain.TestNFT where

import PlutusTx.Prelude

import HydraAuction.PlutusExtras
import Plutus.V1.Ledger.Api (CurrencySymbol, MintingPolicy, TokenName (..), mkMintingPolicyScript)
import Plutus.V1.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Value (AssetClass (..))
import PlutusTx

allowMintingPolicy :: MintingPolicy
allowMintingPolicy =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\_ _ -> ()||])

allowMintingCurrencySymbol :: CurrencySymbol
allowMintingCurrencySymbol = scriptCurrencySymbol allowMintingPolicy

allowMintingAssetClass = AssetClass (allowMintingCurrencySymbol, (TokenName emptyByteString))
