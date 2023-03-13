module Main (main) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))

-- HydraAuction imports
import HydraAuction.Delegate (
  DelegateEvent (Start),
  DelegateInput (DelegateEvent),
  DelegateRunnerT,
  delegateStep,
  execDelegateRunner,
 )

timedEventsProducer :: MVar DelegateInput -> IO ()
timedEventsProducer eventChannel = do
  putMVar eventChannel $ DelegateEvent Start

consumer :: MVar DelegateInput -> IO ()
consumer eventChannel =
  -- Either ignored, because infinite loop cannot actually return
  void $ execDelegateRunner go
  where
    go :: DelegateRunnerT IO ()
    go = do
      event <- liftIO $ takeMVar eventChannel
      delegateResponse <- delegateStep event

      -- FIXME: send response to client
      liftIO $ putStrLn $ show delegateResponse
      go

main :: IO ()
main = do
  eventChannel <- newEmptyMVar

  withAsync (timedEventsProducer eventChannel) $ \_ -> do
    consumer eventChannel
