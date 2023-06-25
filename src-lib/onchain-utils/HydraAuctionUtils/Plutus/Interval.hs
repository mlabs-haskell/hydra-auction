module HydraAuctionUtils.Plutus.Interval (
  strictTo,
  rightExclusiveInterval,
  secondsLeftInInterval,
  extendIntervalRight,
) where

-- Prelude imports
import PlutusTx.Prelude
import Prelude (div)

-- Plutus imports

import PlutusLedgerApi.V1.Interval (
  Extended (..),
  Interval (..),
  LowerBound (..),
  UpperBound (..),
  lowerBound,
  strictUpperBound,
 )
import PlutusLedgerApi.V1.Time (POSIXTime (..))

{-# INLINEABLE strictTo #-}
strictTo :: a -> Interval a
strictTo s = Interval (LowerBound NegInf True) (strictUpperBound s)

{-# INLINEABLE rightExclusiveInterval #-}
rightExclusiveInterval :: a -> a -> Interval a
rightExclusiveInterval s s' = Interval (lowerBound s) (strictUpperBound s')

{-# INLINEABLE extendIntervalRight #-}
extendIntervalRight :: Interval a -> Interval a
extendIntervalRight (Interval a _) = Interval a (UpperBound PosInf True)

{- | Given a POSIXTime, and an 'Interval' this function computes
   the truncated difference in seconds to the 'UpperBound' of the passed 'Interval'.
   If the Interval does not have a finite `UpperBound`,
   or if the given time is past the finite `UpperBound` the function will return Nothing.
   Note that this function is not and should not be used on-chain
-}
{-# INLINEABLE secondsLeftInInterval #-}
secondsLeftInInterval :: POSIXTime -> Interval POSIXTime -> Maybe Integer
secondsLeftInInterval (POSIXTime now) (Interval _ (UpperBound (Finite (POSIXTime t)) inclusive))
  | now < t =
      Just $ (t - now - if inclusive then 0 else 1) `div` 1000
secondsLeftInInterval _ _ = Nothing
