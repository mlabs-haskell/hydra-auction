module HydraAuctionUtils.Tx.Utxo (
  filterAdaOnlyUtxo,
  filterUtxoByCurrencySymbols,
  filterNonFuelUtxo,
) where

-- Prelude imports

import PlutusTx.Prelude (emptyByteString)
import Prelude

-- Haskell imports
import Data.List (sort)
import Data.Map qualified as Map

-- Plutus imports
import Plutus.V1.Ledger.Value (
  CurrencySymbol (..),
  symbols,
 )
import Plutus.V2.Ledger.Api (TxOut (txOutValue))

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports

import Hydra.Cardano.Api (UTxO, UTxO' (UTxO), toPlutusTxOut)
import Hydra.Chain.Direct.Util (isMarkedOutput)

filterAdaOnlyUtxo :: UTxO -> UTxO
filterAdaOnlyUtxo = filterUtxoByCurrencySymbols [CurrencySymbol emptyByteString]

filterUtxoByCurrencySymbols :: [CurrencySymbol] -> UTxO -> UTxO
filterUtxoByCurrencySymbols symbolsToMatch = UTxO.filter hasExactlySymbols
  where
    hasExactlySymbols x =
      (sort . symbols . txOutValue <$> toPlutusTxOut x)
        == Just (sort symbolsToMatch)

-- | Fuel is Utxo mark used by Hydra Node
filterNonFuelUtxo :: UTxO.UTxO -> UTxO.UTxO
filterNonFuelUtxo =
  UTxO . snd . Map.partition isMarkedOutput . UTxO.toMap
