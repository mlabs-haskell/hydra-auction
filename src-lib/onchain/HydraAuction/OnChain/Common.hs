{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-specialise #-}

module HydraAuction.OnChain.Common (
  checkNonAdaOutputsNum,
  minAuctionFee,
  validAuctionTerms,
  checkVoucherExpiredOrLater,
  checkInterval,
  secondsLeftInInterval,
  stageToInterval,
  strictTo,
) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports
import PlutusLedgerApi.V1.Interval (Interval (..), contains, from)
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusLedgerApi.V2.Contexts (TxInfo (..))

-- Hydra auction imports
import HydraAuction.Types (AuctionStage (..), AuctionTerms (..))
import HydraAuctionUtils.Plutus.Interval (
  rightExclusiveInterval,
  secondsLeftInInterval,
  strictTo,
 )
import HydraAuctionUtils.Plutus.TxOut (checkNonAdaOutputsNum)
import HydraAuctionUtils.Types.Natural (naturalToInt)

{-# INLINEABLE minAuctionFee #-}
minAuctionFee :: Integer
minAuctionFee = 2_000_000

{-# INLINEABLE stageToInterval #-}
stageToInterval :: AuctionTerms -> AuctionStage -> Interval POSIXTime
stageToInterval terms stage = case stage of
  AnnouncedStage -> strictTo (biddingStart terms)
  BiddingStartedStage -> rightExclusiveInterval (biddingStart terms) (biddingEnd terms)
  BiddingEndedStage -> rightExclusiveInterval (biddingEnd terms) (voucherExpiry terms)
  VoucherExpiredStage -> rightExclusiveInterval (voucherExpiry terms) (cleanup terms)
  CleanupStage -> from (cleanup terms)

{-# INLINEABLE checkInterval #-}
checkInterval :: AuctionTerms -> AuctionStage -> TxInfo -> Bool
checkInterval terms stage info =
  traceIfFalse "Wrong interval for transaction (checkInterval)" $
    contains (stageToInterval terms stage) (txInfoValidRange info)

{-# INLINEABLE checkVoucherExpiredOrLater #-}
checkVoucherExpiredOrLater :: AuctionTerms -> TxInfo -> Bool
checkVoucherExpiredOrLater terms info =
  traceIfFalse "Wrong interval for transaction (checkcheckVoucherExpiredOrLater)" $
    contains (from (voucherExpiry terms)) (txInfoValidRange info)

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
