module HydraAuctionUtils.Plutus.Interval (
  strictTo,
  rightExclusiveInterval,
  extendIntervalRight,
) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports
import PlutusLedgerApi.V1.Interval (
  Extended (..),
  Interval (..),
  LowerBound (..),
  UpperBound (..),
  lowerBound,
  strictUpperBound,
 )

{-# INLINEABLE strictTo #-}
strictTo :: a -> Interval a
strictTo s = Interval (LowerBound NegInf True) (strictUpperBound s)

{-# INLINEABLE rightExclusiveInterval #-}
rightExclusiveInterval :: a -> a -> Interval a
rightExclusiveInterval s s' = Interval (lowerBound s) (strictUpperBound s')

{-# INLINEABLE extendIntervalRight #-}
extendIntervalRight :: Interval a -> Interval a
extendIntervalRight (Interval a _) = Interval a (UpperBound PosInf True)
