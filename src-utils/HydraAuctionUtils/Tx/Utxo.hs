module HydraAuctionUtils.Tx.Utxo (
  txOutIsAdaOnly,
  filterAdaOnlyUtxo,
  filterUtxoByCurrencySymbols,
  filterNonFuelUtxo,
  filterNotAdaOnlyUtxo,
  extractSingleUtxo,
) where

-- Prelude imports

import HydraAuctionUtils.Prelude
import PlutusTx.Prelude (emptyByteString)

-- Haskell imports
import Data.List (sort)
import Data.Map qualified as Map

-- Plutus imports
import PlutusLedgerApi.V1.Value (
  CurrencySymbol (..),
  symbols,
 )
import PlutusLedgerApi.V2.Tx (txOutValue)

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports

import Data.Maybe (listToMaybe)
import Hydra.Cardano.Api (
  CtxUTxO,
  TxOut,
  UTxO,
  UTxO' (UTxO),
  toPlutusTxOut,
 )
import Hydra.Chain.Direct.Util (isMarkedOutput)

filterNotAdaOnlyUtxo :: UTxO -> UTxO
filterNotAdaOnlyUtxo =
  UTxO.filter (not . txOutIsAdaOnly)

txOutIsAdaOnly :: TxOut CtxUTxO -> Bool
txOutIsAdaOnly = hasExactlySymbols [CurrencySymbol emptyByteString]

filterAdaOnlyUtxo :: UTxO -> UTxO
filterAdaOnlyUtxo = filterUtxoByCurrencySymbols [CurrencySymbol emptyByteString]

filterUtxoByCurrencySymbols :: [CurrencySymbol] -> UTxO -> UTxO
filterUtxoByCurrencySymbols symbolsToMatch =
  UTxO.filter $ hasExactlySymbols symbolsToMatch

hasExactlySymbols :: [CurrencySymbol] -> TxOut CtxUTxO -> Bool
hasExactlySymbols symbolsToMatch x =
  (sort . symbols . txOutValue <$> toPlutusTxOut x)
    == Just (sort symbolsToMatch)

-- | Fuel is Utxo mark used by Hydra Node
filterNonFuelUtxo :: UTxO.UTxO -> UTxO.UTxO
filterNonFuelUtxo =
  UTxO . snd . Map.partition isMarkedOutput . UTxO.toMap

extractSingleUtxo :: UTxO.UTxO -> Maybe UTxO.UTxO
extractSingleUtxo utxo = do
  pair <- listToMaybe $ UTxO.pairs utxo
  return $ UTxO.fromPairs [pair]
