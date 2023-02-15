module HydraAuction.OnChain.TestNFT (allowMintingPolicy, allowMintingCurrencySymbol, allowMintingAssetClass) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports
import Plutus.V1.Ledger.Value (AssetClass (..))
import Plutus.V2.Ledger.Api (CurrencySymbol, MintingPolicy, TokenName (..), mkMintingPolicyScript)
import PlutusTx

-- Hydra auction imports
import HydraAuction.Plutus.Extras

allowMintingPolicy :: MintingPolicy
allowMintingPolicy =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\_ _ -> ()||])

allowMintingCurrencySymbol :: CurrencySymbol
allowMintingCurrencySymbol = scriptCurrencySymbol allowMintingPolicy

allowMintingAssetClass :: AssetClass
allowMintingAssetClass = AssetClass (allowMintingCurrencySymbol, TokenName emptyByteString)
