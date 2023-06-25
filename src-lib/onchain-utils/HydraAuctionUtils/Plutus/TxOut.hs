module HydraAuctionUtils.Plutus.TxOut (
  byAddress,
  checkNonAdaOutputsNum,
  decodeOutputDatum,
  isNotAdaOnlyOutput,
  lovelaceOfOutput,
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
 )
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (..),
  TxInfo (..),
  TxOut (..),
  findDatum,
 )
import PlutusLedgerApi.V2.Tx (OutputDatum (..))
import PlutusTx qualified
import PlutusTx.IsData.Class (fromBuiltinData)

-- TxOut stuff

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

{-# INLINEABLE checkNonAdaOutputsNum #-}
checkNonAdaOutputsNum :: ScriptContext -> Integer -> Bool
checkNonAdaOutputsNum context expectedNum =
  traceIfFalse "Wrong outputs number for tx" $
    length (filter isNotAdaOnlyOutput outputs) == expectedNum
  where
    outputs = txInfoOutputs $ scriptContextTxInfo context
