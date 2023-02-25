module HydraAuction.OnChain.TestNFT (testNftPolicy, testNftCurrencySymbol, testNftAssetClass, testNftTokenName) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports
import Plutus.V1.Ledger.Value (AssetClass (..))
import Plutus.V2.Ledger.Api (CurrencySymbol, MintingPolicy, TokenName (..), mkMintingPolicyScript)
import PlutusTx qualified

-- Hydra auction imports
import HydraAuction.Plutus.Extras (scriptCurrencySymbol)

testNftPolicy :: MintingPolicy
testNftPolicy =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\_ _ -> ()||])

testNftCurrencySymbol :: CurrencySymbol
testNftCurrencySymbol = scriptCurrencySymbol testNftPolicy

testNftTokenName :: TokenName
testNftTokenName = TokenName "Mona Lisa by Leonardo da Vinci"

testNftAssetClass :: AssetClass
testNftAssetClass =
  AssetClass (testNftCurrencySymbol, testNftTokenName)
