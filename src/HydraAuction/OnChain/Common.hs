{-# LANGUAGE RecordWildCards #-}

module HydraAuction.OnChain.Common (minAuctionFee, validAuctionTerms, decodeOutputDatum, byAddress, lovelaceOfOutput) where

import PlutusTx.Prelude

import HydraAuction.Types
import Plutus.V1.Ledger.Value (assetClass, assetClassValueOf)
import Plutus.V2.Ledger.Api (Address, CurrencySymbol (..), OutputDatum (..), POSIXTime (..), TokenName (..), fromBuiltinData, getDatum)
import Plutus.V2.Ledger.Contexts (TxInfo, TxOut, findDatum, txOutAddress, txOutDatum, txOutValue)
import PlutusTx qualified

{-# INLINEABLE minAuctionFee #-}
minAuctionFee :: Integer
minAuctionFee = 2_000_000

{-# INLINEABLE validAuctionTerms' #-}
validAuctionTerms' :: AuctionTerms -> POSIXTime -> Bool
validAuctionTerms' AuctionTerms {..} announcementTxValidityUpperBound =
  announcementTxValidityUpperBound < biddingStart
    && biddingStart < biddingEnd
    && biddingEnd < voucherExpiry
    && voucherExpiry < cleanup
    && naturalToInt minimumBidIncrement > 0
    && startingBid > auctionFee
    && naturalToInt auctionFee > length delegates * minAuctionFee
    && length delegates > 0
    && modulo (naturalToInt auctionFee) (length delegates) == 0

-- FIXME: check interval from TxInfo
{-# INLINEABLE validAuctionTerms #-}
validAuctionTerms :: AuctionTerms -> Bool
validAuctionTerms terms = validAuctionTerms' terms (POSIXTime 0)

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
