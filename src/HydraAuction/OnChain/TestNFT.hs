{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

module HydraAuction.OnChain.TestNFT (
  allowMintingAssetClass,
  allowMintingCurrencySymbol,
  allowMintingPolicy,
) where

import PlutusTx.Prelude

import HydraAuction.PlutusExtras
import Plutus.V1.Ledger.Value (AssetClass (..))
import Plutus.V2.Ledger.Api (CurrencySymbol, MintingPolicy, TokenName (..), mkMintingPolicyScript)
import PlutusTx

allowMintingPolicy :: MintingPolicy
allowMintingPolicy =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\_ _ -> ()||])

allowMintingCurrencySymbol :: CurrencySymbol
allowMintingCurrencySymbol = scriptCurrencySymbol allowMintingPolicy

allowMintingAssetClass :: AssetClass
allowMintingAssetClass = AssetClass (allowMintingCurrencySymbol, TokenName emptyByteString)
