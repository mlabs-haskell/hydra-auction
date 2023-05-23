module HydraAuctionUtils.Plutus (
  byAddress,
  decodeOutputDatum,
  isNotAdaOnlyOutput,
  lovelaceOfOutput,
  nothingForged,
) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports

import PlutusLedgerApi.V1.Address (Address)
import PlutusLedgerApi.V1.Scripts (Datum (getDatum))
import PlutusLedgerApi.V1.Value (
  CurrencySymbol (..),
  TokenName (..),
  Value (..),
  assetClass,
  assetClassValueOf,
  isZero,
 )
import PlutusLedgerApi.V2.Contexts (
  TxInfo (..),
  TxOut (..),
  findDatum,
 )
import PlutusLedgerApi.V2.Tx (
  OutputDatum (..),
 )
import PlutusTx qualified
import PlutusTx.IsData.Class (fromBuiltinData)

{-# INLINEABLE decodeOutputDatum #-}
decodeOutputDatum :: PlutusTx.FromData a => TxInfo -> TxOut -> Maybe a
decodeOutputDatum info output = do
  datum <- case txOutDatum output of
    NoOutputDatum ->
      Nothing
    OutputDatumHash hash ->
      findDatum hash info
    OutputDatum d ->
      Just d
  fromBuiltinData $ getDatum datum

{-# INLINEABLE byAddress #-}
byAddress :: Address -> [TxOut] -> [TxOut]
byAddress address = filter (\o -> txOutAddress o == address)

{-# INLINEABLE isNotAdaOnlyOutput #-}
isNotAdaOnlyOutput :: TxOut -> Bool
isNotAdaOnlyOutput output =
  let value = txOutValue output
   in length (getValue value) > 1

-- XXX: PlutusLedgerApi.V1.Ada module requires more dependencies
lovelaceOfOutput :: TxOut -> Integer
lovelaceOfOutput output = assetClassValueOf (txOutValue output) ac
  where
    ac = assetClass (CurrencySymbol emptyByteString) (TokenName emptyByteString)

{-# INLINEABLE nothingForged #-}
nothingForged :: TxInfo -> Bool
nothingForged info = traceIfFalse "Something was forged" (isZero $ txInfoMint info)
