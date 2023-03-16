{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-specialise #-}

module HydraAuction.OnChain.Common (
  minAuctionFee,
  validAuctionTerms,
  decodeOutputDatum,
  byAddress,
  checkVoucherExpiredOrLater,
  lovelaceOfOutput,
  nothingForged,
  checkInterval,
  secondsLeftInInterval,
  stageToInterval,
) where

-- Prelude imports
import PlutusTx.Prelude
import Prelude (div)

-- Plutus imports
import Plutus.V1.Ledger.Interval (Extended (..), Interval (..), UpperBound (..), contains, from, interval, to)
import Plutus.V1.Ledger.Time (POSIXTime (..))
import Plutus.V1.Ledger.Value (assetClass, assetClassValueOf, isZero)
import Plutus.V2.Ledger.Api (
  Address,
  CurrencySymbol (..),
  OutputDatum (..),
  TokenName (..),
  fromBuiltinData,
  getDatum,
  txInfoValidRange,
 )
import Plutus.V2.Ledger.Contexts (TxInfo, TxOut, findDatum, txInfoMint, txOutAddress, txOutDatum, txOutValue)
import PlutusTx qualified

-- Hydra auction imports
import HydraAuction.Types (AuctionStage (..), AuctionTerms (..), naturalToInt)

{-# INLINEABLE minAuctionFee #-}
minAuctionFee :: Integer
minAuctionFee = 2_000_000

{-# INLINEABLE stageToInterval #-}
stageToInterval :: AuctionTerms -> AuctionStage -> Interval POSIXTime
stageToInterval terms stage = case stage of
  AnnouncedStage -> to (biddingStart terms)
  BiddingStartedStage -> interval (biddingStart terms) (biddingEnd terms)
  BiddingEndedStage -> interval (biddingEnd terms) (voucherExpiry terms)
  VoucherExpiredStage -> interval (voucherExpiry terms) (cleanup terms)
  CleanupStage -> from (cleanup terms)

{-# INLINEABLE checkInterval #-}
checkInterval :: AuctionTerms -> AuctionStage -> TxInfo -> Bool
checkInterval terms stage info =
  traceIfFalse "Wrong interval for transaction" $
    contains (stageToInterval terms stage) (txInfoValidRange info)

{-# INLINEABLE checkVoucherExpiredOrLater #-}
checkVoucherExpiredOrLater :: AuctionTerms -> TxInfo -> Bool
checkVoucherExpiredOrLater terms info =
  traceIfFalse "Wrong interval for transaction" $
    contains (from (voucherExpiry terms)) (txInfoValidRange info)

{- | Given a POSIXTime, and an 'Interval' this function computes
   the truncated difference in seconds to the 'UpperBound' of the passed 'Interval'.
   If the Interval does not have a finite `UpperBound`,
   or if the given time is past the finite `UpperBound` the function will return Nothing.
   Note that this function is not and should not be used on-chain
-}
secondsLeftInInterval :: POSIXTime -> Interval POSIXTime -> Maybe Integer
secondsLeftInInterval (POSIXTime now) (Interval _ (UpperBound (Finite (POSIXTime t)) inclusive)) | now < t = Just $ (t - now - if inclusive then 0 else 1) `div` 1000
secondsLeftInInterval _ _ = Nothing

{-# INLINEABLE validAuctionTerms #-}
validAuctionTerms :: AuctionTerms -> Bool
validAuctionTerms AuctionTerms {..} =
  -- VAT1 was removed
  traceIfFalse "VAT2" (biddingStart < biddingEnd)
    && traceIfFalse "VAT3" (biddingEnd < voucherExpiry)
    && traceIfFalse "VAT4" (voucherExpiry < cleanup)
    && traceIfFalse "VAT5" (naturalToInt minimumBidIncrement > 0)
    && traceIfFalse "VAT6" (naturalToInt startingBid > naturalToInt auctionFeePerDelegate * length delegates)
    && traceIfFalse "VAT7" (naturalToInt auctionFeePerDelegate > minAuctionFee)
    && traceIfFalse "VAT8" (length delegates > 0)

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

{-# INLINEABLE nothingForged #-}
nothingForged :: TxInfo -> Bool
nothingForged info = traceIfFalse "Something was forged" (isZero $ txInfoMint info)
