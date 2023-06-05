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

-- Hydra auction
import CLI.Config (
  AuctionName (..),
  CliEnhancedAuctionTerms (..),
  readCliEnhancedAuctionTerms,
 )
import Data.IORef (IORef, readIORef)
import HydraAuction.Delegate.Interface (
  DelegateState (..),
  InitializedState (..),
  OpenHeadUtxo (..),
 )
import HydraAuction.OnChain.Common (secondsLeftInInterval, stageToInterval)
import HydraAuction.Tx.Common (currentAuctionStage)
import HydraAuctionUtils.Time (currentPlutusPOSIXTime)

watchAuction :: AuctionName -> IORef DelegateState -> IO ()
watchAuction auctionName currentDelegateStateRef = do
  clearScreen
  setCursorPosition 0 0

  mEnhancedTerms <- readCliEnhancedAuctionTerms auctionName

  delegateState <- readIORef currentDelegateStateRef
  showDelegateState delegateState

  case mEnhancedTerms of
    Nothing ->
      putStrLn $ "Auction " <> show auctionName <> " does not exist."
    Just CliEnhancedAuctionTerms {sellerActor, terms} -> do
      currentTime <- getCurrentTime
      currentPosixTime <- currentPlutusPOSIXTime
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
  where
    showDelegateState delegateState = case delegateState of
      Initialized _ initializedState ->
        let
          showedState = case initializedState of
            Open utxoState _ -> "Open " <> show (standingBidTerms utxoState)
            _ -> show initializedState
         in
          putStrLn $ "Delegate state: " <> showedState
      _ -> putStrLn "Delegate is not initialized yet"
