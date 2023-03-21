module Main (main) where

-- Prelude imports
import Prelude

-- Haskell imports

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))

-- HydraAuction imports
import HydraAuction.Delegate (
  DelegateEvent (Start),
  DelegateInput (DelegateEvent),
  DelegateRunnerT,
  delegateStep,
  execDelegateRunnerT,
 )

eventsProducer :: Bool -> DelegateRunnerT IO (Maybe DelegateEvent)
eventsProducer thisIsFirstRun =
  if thisIsFirstRun
    then return $ Just Start
    else return Nothing

consumer :: Maybe DelegateInput -> DelegateRunnerT IO ()
consumer mInput = case mInput of
  Just input -> do
    delegateResponse <- delegateStep input
    -- FIXME: send response to client
    liftIO $
      putStrLn $
        "Delegate responses for input: " <> show delegateResponse
  Nothing -> return ()

delegateTick :: Int
delegateTick = 1_000

main :: IO ()
main = do
  -- FIXME: cover either case. It probably should not be transformer.
  _ <- execDelegateRunnerT $ go True
  return ()
  where
    go thisIsFirstRun = do
      mEvent <- eventsProducer thisIsFirstRun
      consumer (DelegateEvent <$> mEvent)
      liftIO $ threadDelay delegateTick
      go False
