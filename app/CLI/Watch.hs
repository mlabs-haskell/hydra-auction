module CLI.Watch (
  watchAuction,
) where

-- Prelude
import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Concurrent (threadDelay)
import Data.IORef (IORef, readIORef)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Console.ANSI

-- Hydra auction

import CLI.Config (
  AuctionName (..),
  CliEnhancedAuctionTerms (..),
  readCliEnhancedAuctionTerms,
 )
import HydraAuction.Delegate.Interface (
  DelegateState (..),
  InitializedState (..),
  OpenHeadUtxo (..),
 )
import HydraAuction.OnChain.Common (secondsLeftInInterval, stageToInterval)
import HydraAuction.Tx.Common (currentAuctionStage)
import HydraAuctionUtils.L1.Runner (dockerNode, executeL1RunnerWithNode)
import HydraAuctionUtils.Monads (queryCurrentSlot)
import HydraAuctionUtils.Time (currentPlutusPOSIXTime)

watchAuction :: AuctionName -> IORef DelegateState -> IO ()
watchAuction auctionName currentDelegateStateRef = do
  clearScreen
  setCursorPosition 0 0

  mEnhancedTerms <- readCliEnhancedAuctionTerms auctionName

  currentTime <- getCurrentTime
  putStrLn $ showTime currentTime
  void $ trySome $ do
    currentSlot <- executeL1RunnerWithNode dockerNode queryCurrentSlot
    putStrLn $ "Current L1 slot: " <> show currentSlot

  void $ showDelegateState <$> readIORef currentDelegateStateRef

  case mEnhancedTerms of
    Nothing ->
      putStrLn $ "Auction " <> show auctionName <> " does not exist."
    Just CliEnhancedAuctionTerms {sellerActor, terms} -> do
      currentPosixTime <- currentPlutusPOSIXTime
      currentStage <- currentAuctionStage terms
      let
        mSecsLeft = secondsLeftInInterval currentPosixTime (stageToInterval terms currentStage)
      putStrLn $ "Auction: " <> show auctionName
      putStrLn $ "Seller: " <> show sellerActor
      putStrLn $
        "Current stage: "
          <> show currentStage
          <> case mSecsLeft of
            Nothing -> mempty
            Just s -> "\n" <> show s <> " seconds left until next stage"

  threadDelay 100_000
  watchAuction auctionName currentDelegateStateRef
  where
    showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
    showDelegateState delegateState = case delegateState of
      Initialized _ initializedState ->
        let
          showedState = case initializedState of
            Open utxoState _ -> "Open " <> show (standingBidTerms utxoState)
            _ -> show initializedState
         in
          putStrLn $ "Delegate state: " <> showedState
      _ -> putStrLn "Delegate is not initialized yet"
