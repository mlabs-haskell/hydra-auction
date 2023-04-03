module HydraAuctionUtils.Tx.Utxo (
  filterAdaOnlyUtxo,
  filterUtxoByCurrencySymbols,
) where

-- Prelude imports

import PlutusTx.Prelude (emptyByteString)
import Prelude

-- Haskell imports
import Data.List (sort)

-- Plutus imports
import Plutus.V1.Ledger.Value (
  CurrencySymbol (..),
  symbols,
 )

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports
import Hydra.Cardano.Api (UTxO, toPlutusTxOut)
import Plutus.V2.Ledger.Api (TxOut (txOutValue))

filterAdaOnlyUtxo :: UTxO -> UTxO
filterAdaOnlyUtxo = filterUtxoByCurrencySymbols [CurrencySymbol emptyByteString]

filterUtxoByCurrencySymbols :: [CurrencySymbol] -> UTxO -> UTxO
filterUtxoByCurrencySymbols symbolsToMatch = UTxO.filter hasExactlySymbols
  where
    hasExactlySymbols x =
      (sort . symbols . txOutValue <$> toPlutusTxOut x)
        == Just (sort symbolsToMatch)
