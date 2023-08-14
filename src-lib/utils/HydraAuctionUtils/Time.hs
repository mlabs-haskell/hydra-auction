module HydraAuctionUtils.Time (
  posixTimeToUTC,
  currentTimeSeconds,
  currentTimeMilliseconds,
  currentPlutusPOSIXTime,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Monad.TimeMachine (MonadTime (getCurrentTime))
import Data.Time (UTCTime, picosecondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock.POSIX qualified as POSIXTime

-- Plutus imports
import PlutusLedgerApi.V1.Time (POSIXTime (..))

posixTimeToUTC :: POSIXTime -> UTCTime
posixTimeToUTC ptime = posixSecondsToUTCTime fractionalSeconds
  where
    fractionalSeconds = fromInteger (getPOSIXTime ptime) / 1_000

currentTimeSeconds :: MonadTime timedMonad => timedMonad Integer
currentTimeSeconds =
  round . POSIXTime.utcTimeToPOSIXSeconds <$> getCurrentTime

currentTimeMilliseconds :: MonadTime timedMonad => timedMonad Integer
currentTimeMilliseconds =
  round . (* 1000) . POSIXTime.utcTimeToPOSIXSeconds <$> getCurrentTime

currentPlutusPOSIXTime :: MonadTime timedMonad => timedMonad POSIXTime
currentPlutusPOSIXTime = POSIXTime <$> currentTimeMilliseconds
