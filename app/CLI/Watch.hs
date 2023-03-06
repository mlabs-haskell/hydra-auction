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

-- Hydra auction CLI imports
import CLI.Config (
  AuctionName (..),
  CliEnhancedAuctionTerms (..),
  readCliEnhancedAuctionTerms,
 )

watchAuction :: AuctionName -> IO ()
watchAuction auctionName = do
  clearScreen
  setCursorPosition 0 0

  mEnhancedTerms <- readCliEnhancedAuctionTerms auctionName

  case mEnhancedTerms of
    Nothing ->
      putStrLn $ "Auction " <> show auctionName <> " does not exist."
    Just CliEnhancedAuctionTerms {sellerActor} -> do
      currentTime <- getCurrentTime
      let showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
      putStrLn $ showTime currentTime
      putStrLn $ "Auction: " <> show auctionName
      putStrLn $ "Seller: " <> show sellerActor
      putStrLn $
        "Current stage: "
          <> "[TODO: What is the current stage "
          <> "and how many seconds are left (if applicable)?]"
      putStrLn $
        "Standing bid held by: "
          <> "[TODO: Who submitted the standing bid?]"

  threadDelay 0.2
  watchAuction auctionName
