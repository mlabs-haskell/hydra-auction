module HydraAuctionUtils.Plutus where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports

import Plutus.V1.Ledger.Value (assetClass, assetClassValueOf, isZero)
import Plutus.V2.Ledger.Api (
  Address,
  CurrencySymbol (..),
  OutputDatum (..),
  TokenName (..),
  Value (..),
  fromBuiltinData,
  getDatum,
 )
import Plutus.V2.Ledger.Contexts (
  TxInfo (..),
  TxOut (..),
  findDatum,
 )
import PlutusTx qualified

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

-- XXX: Plutus.V1.Ledger.Ada module requires more dependencies
lovelaceOfOutput :: TxOut -> Integer
lovelaceOfOutput output = assetClassValueOf (txOutValue output) ac
  where
    ac = assetClass (CurrencySymbol emptyByteString) (TokenName emptyByteString)

{-# INLINEABLE nothingForged #-}
nothingForged :: TxInfo -> Bool
nothingForged info = traceIfFalse "Something was forged" (isZero $ txInfoMint info)
