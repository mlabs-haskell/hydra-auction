{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-specialise #-}

module HydraAuction.OnChain.Common (
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
import Prelude (div)

-- Plutus imports
import PlutusLedgerApi.V1.Interval (Extended (..), Interval (..), UpperBound (..), contains, from)
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusLedgerApi.V2.Contexts (TxInfo (..))

-- Hydra auction imports
import HydraAuction.Types (AuctionStage (..), AuctionTerms (..))
import HydraAuctionUtils.Plutus (rightExclusiveInterval, strictTo)
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

{- | Given a POSIXTime, and an 'Interval' this function computes
   the truncated difference in seconds to the 'UpperBound' of the passed 'Interval'.
   If the Interval does not have a finite `UpperBound`,
   or if the given time is past the finite `UpperBound` the function will return Nothing.
   Note that this function is not and should not be used on-chain
-}
secondsLeftInInterval :: POSIXTime -> Interval POSIXTime -> Maybe Integer
secondsLeftInInterval (POSIXTime now) (Interval _ (UpperBound (Finite (POSIXTime t)) inclusive))
  | now < t =
      Just $ (t - now - if inclusive then 0 else 1) `div` 1000
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