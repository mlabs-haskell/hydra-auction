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
import Data.Time (UTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock.POSIX qualified as POSIXTime

-- Plutus imports
import PlutusLedgerApi.V1.Time (POSIXTime (..))

posixTimeToUTC :: POSIXTime -> UTCTime
posixTimeToUTC ptime = posixSecondsToUTCTime ndtime
  where
    timeInSeconds = getPOSIXTime ptime `div` 1000
    ndtime = secondsToNominalDiffTime $ fromInteger timeInSeconds

currentTimeSeconds :: MonadTime timedMonad => timedMonad Integer
currentTimeSeconds =
  round . POSIXTime.utcTimeToPOSIXSeconds <$> getCurrentTime

currentTimeMilliseconds :: MonadTime timedMonad => timedMonad Integer
currentTimeMilliseconds =
  round . (* 1000) . POSIXTime.utcTimeToPOSIXSeconds <$> getCurrentTime

currentPlutusPOSIXTime :: MonadTime timedMonad => timedMonad POSIXTime
currentPlutusPOSIXTime = POSIXTime <$> currentTimeMilliseconds
