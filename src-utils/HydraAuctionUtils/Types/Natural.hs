module HydraAuctionUtils.Types.Natural (
  Natural,
  intToNatural,
  naturalToInt,
) where

-- Prelude imports
import PlutusTx.Prelude
import Prelude qualified

-- Haskell imports

import Control.Monad ((<=<))
import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (parseJSON), ToJSON)
import GHC.Generics (Generic)

-- Plutus imports

import PlutusTx qualified
import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )

-- Custom Natural

-- Not using natural-numbers package, cuz it does not provide Integer conversion methods,
-- which are compatible with PlutusTx

newtype Natural = Natural Integer
  deriving stock (Generic, Prelude.Show, Prelude.Eq, Prelude.Ord)
  deriving newtype (Eq, Ord, AdditiveSemigroup, MultiplicativeSemigroup, ToJSON)

instance UnsafeFromData Natural where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData =
    maybe (error ()) id . intToNatural . unsafeFromBuiltinData

instance ToData Natural where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = toBuiltinData . naturalToInt

instance FromData Natural where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d = do
    i <- fromBuiltinData d
    intToNatural i

{-# INLINEABLE intToNatural #-}
intToNatural :: Integer -> Maybe Natural
intToNatural x
  | x > 0 = Just $ Natural x
  | otherwise = Nothing

{-# INLINEABLE naturalToInt #-}
naturalToInt :: Natural -> Integer
naturalToInt (Natural i) = i

PlutusTx.makeLift ''Natural

instance FromJSON Natural where
  parseJSON =
    maybe (fail "Integer is not natural") return . intToNatural <=< parseJSON
