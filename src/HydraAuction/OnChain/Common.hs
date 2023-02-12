{-# LANGUAGE RecordWildCards #-}

module HydraAuction.OnChain.Common (minAuctionFee, validAuctionTerms, decodeOutputDatum, byAddress, lovelaceOfOutput) where

import PlutusTx.Prelude

import HydraAuction.Types
import Plutus.V1.Ledger.Value (assetClass, assetClassValueOf)
import Plutus.V2.Ledger.Api (Address, CurrencySymbol (..), OutputDatum (..), TokenName (..), fromBuiltinData, getDatum)
import Plutus.V2.Ledger.Contexts (TxInfo, TxOut, findDatum, txOutAddress, txOutDatum, txOutValue)
import PlutusTx qualified

{-# INLINEABLE minAuctionFee #-}
minAuctionFee :: Integer
minAuctionFee = 2_000_000

{-# INLINEABLE validAuctionTerms #-}
validAuctionTerms :: AuctionTerms -> Bool
validAuctionTerms AuctionTerms {..} =
  -- VAT1 was removed
  traceIfFalse "VAT2" (biddingStart < biddingEnd)
    && traceIfFalse "VAT3" (biddingEnd < voucherExpiry)
    && traceIfFalse "VAT4" (voucherExpiry < cleanup)
    && traceIfFalse "VAT5" (naturalToInt minimumBidIncrement > 0)
    && traceIfFalse "VAT6" (startingBid > auctionFee)
    && traceIfFalse "VAT7" (naturalToInt auctionFee > length delegates * minAuctionFee)
    && traceIfFalse "VAT8" (length delegates > 0)
    && traceIfFalse "VAT9" (modulo (naturalToInt auctionFee) (length delegates) == 0)

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

-- XXX: Plutus.V1.Ledger.Ada module requires more dependencies
lovelaceOfOutput :: TxOut -> Integer
lovelaceOfOutput output = assetClassValueOf (txOutValue output) ac
  where
    ac = assetClass (CurrencySymbol emptyByteString) (TokenName emptyByteString)
