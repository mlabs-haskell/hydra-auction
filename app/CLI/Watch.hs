module CLI.Watch (
  watchAuction,
) where

-- Prelude
import Hydra.Prelude (MonadDelay (threadDelay))
import Prelude

-- Haskell imports
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Console.ANSI

-- Plutus imports
import Plutus.V2.Ledger.Api (POSIXTime (..))

-- Hydra auction
import CLI.Config (
  AuctionName (..),
  CliEnhancedAuctionTerms (..),
  readCliEnhancedAuctionTerms,
 )
import Data.IORef (IORef, readIORef)
import HydraAuction.Delegate.Interface (DelegateState (..))
import HydraAuction.OnChain.Common (secondsLeftInInterval, stageToInterval)
import HydraAuction.Tx.Common (currentAuctionStage, currentTimeMilliseconds)

watchAuction :: AuctionName -> IORef DelegateState -> IO ()
watchAuction auctionName currentDelegateStateRef = do
  clearScreen
  setCursorPosition 0 0

  mEnhancedTerms <- readCliEnhancedAuctionTerms auctionName

  delegateState <- readIORef currentDelegateStateRef
  case delegateState of
    Initialized _ initializedState ->
      putStrLn $ "Delegate state: " <> show initializedState
    _ -> putStrLn "Delegate is not initialized yet"

  case mEnhancedTerms of
    Nothing ->
      putStrLn $ "Auction " <> show auctionName <> " does not exist."
    Just CliEnhancedAuctionTerms {sellerActor, terms} -> do
      currentTime <- getCurrentTime
      currentPosixTime <- POSIXTime <$> currentTimeMilliseconds
      currentStage <- currentAuctionStage terms
      let showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
          mSecsLeft = secondsLeftInInterval currentPosixTime (stageToInterval terms currentStage)
      putStrLn $ showTime currentTime
      putStrLn $ "Auction: " <> show auctionName
      putStrLn $ "Seller: " <> show sellerActor
      putStrLn $
        "Current stage: "
          <> show currentStage
          <> case mSecsLeft of
            Nothing -> mempty
            Just s -> "\n" <> show s <> " seconds left until next stage"

  threadDelay 0.2
  watchAuction auctionName currentDelegateStateRef
