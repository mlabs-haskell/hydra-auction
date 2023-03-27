module Main (main) where

-- Prelude imports
import Prelude

-- Haskell imports

import Control.Concurrent (Chan, newChan, readChan, threadDelay, writeChan)
import Control.Concurrent.Async (async)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))

-- Plutus imports
import Plutus.V2.Ledger.Api (POSIXTime (..))

-- HydraAuction imports

import HydraAuction.Delegate (
  DelegateEvent (..),
  DelegateInput (DelegateEvent),
  DelegateRunnerT,
  delegateStep,
  execDelegateRunnerT,
 )
import HydraAuction.Delegate.Interface (DelegateResponse (..))
import HydraAuction.OnChain.Common (secondsLeftInInterval, stageToInterval)
import HydraAuction.Tx.Common (currentAuctionStage, currentTimeMilliseconds)

consumer :: Chan DelegateInput -> DelegateRunnerT IO ()
consumer events = do
  -- This will block until an event appears in the queue
  ev <- liftIO $ readChan events
  delegateResponse <- delegateStep ev
  liftIO $ mbQueueAuctionPhases delegateResponse events
  -- FIXME: send response to client
  liftIO $
    putStrLn $
      "Delegate responses for input: " <> show delegateResponse

mbQueueAuctionPhases :: [DelegateResponse] -> Chan DelegateInput -> IO ()
mbQueueAuctionPhases [AuctionSet terms] events = queueCurrentStage
  where
    queueCurrentStage = do
      currentStage <- currentAuctionStage terms
      writeChan events (DelegateEvent $ AuctionStageStarted currentStage)
      currentPosixTime <- POSIXTime <$> currentTimeMilliseconds
      let mSecsLeft = secondsLeftInInterval currentPosixTime (stageToInterval terms currentStage)
      case mSecsLeft of
        Nothing -> pure ()
        Just s -> do
          void $ async (threadDelay (fromInteger s * 1000) >> queueCurrentStage)
mbQueueAuctionPhases _ _ = pure ()

spawnServer :: Chan DelegateInput -> IO ()
spawnServer _ = forever $ pure ()

main :: IO ()
main = do
  events <- newChan
  -- Write init even in queue
  writeChan events (DelegateEvent Start)
  -- FIXME: cover either case. It probably should not be transformer.
  _ <- execDelegateRunnerT $ forever (consumer events)
  return ()
