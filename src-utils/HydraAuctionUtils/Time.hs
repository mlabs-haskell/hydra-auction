module HydraAuctionUtils.Time (
  currentTimeSeconds,
  currentTimeMilliseconds,
  currentPlutusPOSIXTime,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad.TimeMachine (MonadTime (getCurrentTime))
import Data.Time.Clock.POSIX qualified as POSIXTime

-- Plutus imports
import Plutus.V2.Ledger.Api (
  POSIXTime (..),
 )

currentTimeSeconds :: MonadTime timedMonad => timedMonad Integer
currentTimeSeconds =
  round . POSIXTime.utcTimeToPOSIXSeconds <$> getCurrentTime

currentTimeMilliseconds :: MonadTime timedMonad => timedMonad Integer
currentTimeMilliseconds =
  round . (* 1000) . POSIXTime.utcTimeToPOSIXSeconds <$> getCurrentTime

currentPlutusPOSIXTime :: MonadTime timedMonad => timedMonad POSIXTime
currentPlutusPOSIXTime = POSIXTime <$> currentTimeMilliseconds
