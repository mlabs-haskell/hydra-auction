module HydraAuction.OnChain.TestNFT (testNftPolicy, testNftCurrencySymbol, testNftAssetClass, testNftTokenName) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  CurrencySymbol,
  TokenName (..),
 )
import PlutusTx qualified

-- Hydra auction imports
import HydraAuctionUtils.Extras.Plutus (scriptCurrencySymbol)

testNftPolicy :: SerialisedScript
testNftPolicy =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||\_ _ -> ()||])

testNftCurrencySymbol :: CurrencySymbol
testNftCurrencySymbol = scriptCurrencySymbol testNftPolicy

testNftTokenName :: TokenName
testNftTokenName = TokenName "Mona Lisa by Leonardo da Vinci"

testNftAssetClass :: AssetClass
testNftAssetClass =
  AssetClass (testNftCurrencySymbol, testNftTokenName)
