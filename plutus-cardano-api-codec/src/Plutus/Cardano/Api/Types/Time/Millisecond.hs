module Plutus.Cardano.Api.Types.Time.Millisecond (
  -- POSIX time
  POSIXTimeMilli,
  integerToPOSIXTimeMilli,
  posixTimeMilliToInteger,
  restrictPOSIXTimeMilli,
  fromPOSIXTimeMilli,
  restorePOSIXTime,
  -- UTC time
  UTCTimeMilli,
  restrictUTCTimeMilli,
  fromUTCTimeMilli,
  restoreUTCTime,
  -- UTC <-> POSIX
  posixMilliToUTCMilli,
  utcTimeMilliToPOSIXMilli,
) where

import GHC.Generics (Generic)
import Prelude

import Data.Function ((&))

import Data.Fixed
import Data.Time.Calendar (Day)
import Data.Time.Clock (
  UTCTime (..),
  nominalDiffTimeToSeconds,
  secondsToNominalDiffTime,
 )
import Data.Time.Clock.POSIX (
  POSIXTime,
  posixSecondsToUTCTime,
  utcTimeToPOSIXSeconds,
 )

-- -------------------------------------------------------------------------
-- Posix time
-- -------------------------------------------------------------------------
type POSIXTimeMilli = NominalDiffMilliSeconds

newtype NominalDiffMilliSeconds = NominalDiffMilliSeconds Milli
  deriving stock (Eq, Generic, Ord, Show)

-- | Get the integer number of milliseconds in a POSIX time.
posixTimeMilliToInteger :: POSIXTimeMilli -> Integer
posixTimeMilliToInteger (NominalDiffMilliSeconds (MkFixed x)) = x

-- | Construct a POSIX time from an integer number of milliseconds.
integerToPOSIXTimeMilli :: Integer -> POSIXTimeMilli
integerToPOSIXTimeMilli = NominalDiffMilliSeconds . MkFixed

-- | Restrict a POSIXTime to millisecond resolution.
restrictPOSIXTimeMilli :: POSIXTime -> POSIXTimeMilli
restrictPOSIXTimeMilli posix =
  posix
    & nominalDiffTimeToSeconds
    & realToFrac
    & NominalDiffMilliSeconds

-- | Get a POSIXTime from a PosixTimeMilli by
-- setting the sub-millisecond digits to zero.
fromPOSIXTimeMilli :: POSIXTimeMilli -> POSIXTime
fromPOSIXTimeMilli (NominalDiffMilliSeconds x) =
  realToFrac x
    & secondsToNominalDiffTime

-- | Restore a PosixTimeMilli to picosecond resolution
-- by adding the sub-millisecond digits from an anchor time.
restorePOSIXTime :: POSIXTime -> POSIXTimeMilli -> POSIXTime
restorePOSIXTime anchor posixMilli =
  let
    (_, anchorSubMs) =
      anchor
        & nominalDiffTimeToSeconds
        & picoToMilli
   in
    fromPOSIXTimeMilli posixMilli + secondsToNominalDiffTime anchorSubMs

-- | Round a pico-resolution number down to milli resolution,
-- retaining the pico-resolution remainder.
picoToMilli :: Pico -> (Milli, Pico)
picoToMilli x = (x', r)
  where
    x' = realToFrac x
    r = x - realToFrac x'

-- -------------------------------------------------------------------------
-- UTC
-- -------------------------------------------------------------------------
data UTCTimeMilli = UTCTimeMilli !Day !NominalDiffMilliSeconds
  deriving stock (Eq, Generic, Ord, Show)

-- | Restrict a UTCTime to millisecond resolution.
restrictUTCTimeMilli :: UTCTime -> UTCTimeMilli
restrictUTCTimeMilli (UTCTime d t) =
  UTCTimeMilli d (NominalDiffMilliSeconds (realToFrac t))

-- | Get a UTCTime from a UTCTimeMilli by
-- setting the sub-millisecond digits to zero.
fromUTCTimeMilli :: UTCTimeMilli -> UTCTime
fromUTCTimeMilli (UTCTimeMilli d (NominalDiffMilliSeconds t)) =
  UTCTime d (realToFrac t)

-- | Restore a UTCTimeMilli to picosecond resolution
-- by adding the sub-millisecond digits from an anchor time.
-- This is intended to work with a Cardano SystemStart time
-- as the anchor.
restoreUTCTime :: UTCTime -> UTCTimeMilli -> UTCTime
restoreUTCTime anchor utcMilli =
  let
    anchorPosix = utcTimeToPOSIXSeconds anchor
    posixMilli =
      utcMilli
        & fromUTCTimeMilli
        & utcTimeToPOSIXSeconds
        & restrictPOSIXTimeMilli
   in
    restorePOSIXTime anchorPosix posixMilli
      & posixSecondsToUTCTime

-- -------------------------------------------------------------------------
-- UTC <-> POSIX
-- -------------------------------------------------------------------------
posixMilliToUTCMilli :: POSIXTimeMilli -> UTCTimeMilli
posixMilliToUTCMilli posixTimeMilli =
  posixTimeMilli
    & fromPOSIXTimeMilli
    & posixSecondsToUTCTime
    & restrictUTCTimeMilli

utcTimeMilliToPOSIXMilli :: UTCTimeMilli -> POSIXTimeMilli
utcTimeMilliToPOSIXMilli utcTimeMilli =
  utcTimeMilli
    & fromUTCTimeMilli
    & utcTimeToPOSIXSeconds
    & restrictPOSIXTimeMilli
