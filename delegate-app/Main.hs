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
  DelegateRunnerT,
  delegateEventStep,
  execDelegateRunnerT,
 )
import HydraAuction.Delegate.Interface (DelegateResponse (..))
import HydraAuction.OnChain.Common (secondsLeftInInterval, stageToInterval)
import HydraAuction.Tx.Common (currentAuctionStage, currentTimeMilliseconds)

consumer :: Chan DelegateEvent -> DelegateRunnerT IO ()
consumer delegateEvents = do
  -- This will block until an event appears in the queue
  -- FIXME: support frontend requests too
  delegateEvent <- liftIO $ readChan delegateEvents
  delegateResponse <- delegateEventStep delegateEvent
  liftIO $ mbQueueAuctionPhases delegateResponse delegateEvents
  -- FIXME: send response to client
  liftIO $
    putStrLn $
      "Delegate responses for input: " <> show delegateResponse

mbQueueAuctionPhases :: [DelegateResponse] -> Chan DelegateEvent -> IO ()
mbQueueAuctionPhases [AuctionSet terms] events = void $ async queueCurrentStage
  where
    queueCurrentStage = do
      currentStage <- currentAuctionStage terms
      writeChan events $ AuctionStageStarted currentStage
      currentPosixTime <- POSIXTime <$> currentTimeMilliseconds
      let mSecsLeft = secondsLeftInInterval currentPosixTime (stageToInterval terms currentStage)
      case mSecsLeft of
        Nothing -> pure ()
        Just s -> do
          threadDelay (fromInteger s * 1000)
          queueCurrentStage
mbQueueAuctionPhases _ _ = pure ()

main :: IO ()
main = do
  delegateEvents <- newChan
  -- Write init even in queue
  writeChan delegateEvents Start
  -- FIXME: cover either case. It probably should not be transformer.
  _ <- execDelegateRunnerT $ forever (consumer delegateEvents)
  return ()
