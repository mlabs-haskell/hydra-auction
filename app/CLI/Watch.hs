module CLI.Watch (
  watchAuction,
) where

-- Prelude
import HydraAuctionUtils.Prelude

-- Haskell imports

import Data.IORef (IORef, readIORef)
import Data.Time.Clock (
  UTCTime,
  diffUTCTime,
  getCurrentTime,
  nominalDiffTimeToSeconds,
 )
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (BufferMode (..), hFlush, hSetBuffering, stdout)

-- Hydra imports
import Hydra.Cardano.Api (SlotNo (..))

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
import HydraAuction.Types (BidTerms (..))
import HydraAuctionUtils.Fixture (actorFromPkh)
import HydraAuctionUtils.L1.Runner (dockerNode, executeL1RunnerWithNode)
import HydraAuctionUtils.Monads (queryCurrentSlot)
import HydraAuctionUtils.Time (currentPlutusPOSIXTime)
import HydraAuctionUtils.Types.Natural (naturalToInt)

watchAuction :: AuctionName -> IORef DelegateState -> IO ()
watchAuction x y = do
  seenSlotsCounterRef <- newMVar 0
  currentTime <- getCurrentTime
  lastSeenSlot <- newMVar (0, currentTime)
  watchAuction' (seenSlotsCounterRef, lastSeenSlot) x y

watchAuction' ::
  (MVar Integer, MVar (Integer, UTCTime)) ->
  AuctionName ->
  IORef DelegateState ->
  IO ()
watchAuction' slotsVars auctionName currentDelegateStateRef = do
  -- Flushing once per `watchAuction'` to reduce blinking
  hSetBuffering stdout (BlockBuffering (Just 100500))
  clearScreen
  setCursorPosition 0 0

  printTimeAndSlots
  printDelegateState =<< readIORef currentDelegateStateRef
  printAuctionState

  hFlush stdout
  threadDelay 100_000
  watchAuction' slotsVars auctionName currentDelegateStateRef
  where
    showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
    printDelegateState delegateState = case delegateState of
      Initialized _ initializedState -> do
        showedState <- case initializedState of
          Open utxoState _ -> do
            bidTermsStr <- showBidTerms (standingBidTerms utxoState)
            return $ "Open with bid: " <> bidTermsStr
          _ -> return $ show initializedState
        putStrLn $ "Delegate state: " <> showedState
      _ -> putStrLn "Delegate is not initialized yet"
    showBidTerms (Just terms) = do
      bidder <- actorFromPkh $ bidderPKH terms
      let bidAmountAda :: Double =
            realToFrac (naturalToInt (bidAmount terms)) / 1_000_000
      return $ show bidAmountAda <> " ADA placed by " <> show bidder
    showBidTerms Nothing = return "No bid placed jet"
    printTimeAndSlots = do
      currentTime <- getCurrentTime
      putStrLn $ showTime currentTime
      void $ trySome $ do
        -- FIXME: parametrize
        SlotNo currentSlot' <-
          executeL1RunnerWithNode dockerNode queryCurrentSlot
        let currentSlot = fromIntegral currentSlot'
        (lastSeen, lastSeenTime) <- takeMVar (snd slotsVars)
        if lastSeen /= currentSlot
          then do
            modifyMVar_ (fst slotsVars) (return . (1 +))
            putMVar (snd slotsVars) (currentSlot, currentTime)
          else putMVar (snd slotsVars) (currentSlot, lastSeenTime)
        seen <- readMVar (fst slotsVars)
        let passedSecs =
              nominalDiffTimeToSeconds (diffUTCTime currentTime lastSeenTime)
        putStrLn $ "Current L1 slot: " <> show currentSlot
        putStrLn $ "Which stayed for " <> show passedSecs <> " seconds"
        putStrLn $ "Seen L1 slots so far: " <> show seen
    printAuctionState = do
      mEnhancedTerms <- readCliEnhancedAuctionTerms auctionName
      case mEnhancedTerms of
        Nothing ->
          putStrLn $ "Auction " <> show auctionName <> " does not exist."
        Just CliEnhancedAuctionTerms {sellerActor, terms} -> do
          currentPosixTime <- currentPlutusPOSIXTime
          currentStage <- currentAuctionStage terms
          let
            mSecsLeft =
              secondsLeftInInterval
                currentPosixTime
                (stageToInterval terms currentStage)
          putStrLn $ "Auction: " <> show auctionName
          putStrLn $ "Seller: " <> show sellerActor
          putStrLn $
            "Current stage: "
              <> show currentStage
              <> case mSecsLeft of
                Nothing -> mempty
                Just s -> "\n" <> show s <> " seconds left until next stage"
