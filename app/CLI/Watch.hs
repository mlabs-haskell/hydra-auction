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
import Plutus.V1.Ledger.Interval (Extended (..), Interval (..), UpperBound (..))
import Plutus.V1.Ledger.Time (POSIXTime (..))

-- Hydra auction
import CLI.Config (
  AuctionName (..),
  CliEnhancedAuctionTerms (..),
  readCliEnhancedAuctionTerms,
 )
import HydraAuction.OnChain.Common (stageToInterval)
import HydraAuction.Tx.Common (currentAuctionStage, currentTimeMilliseconds)

watchAuction :: AuctionName -> IO ()
watchAuction auctionName = do
  clearScreen
  setCursorPosition 0 0

  mEnhancedTerms <- readCliEnhancedAuctionTerms auctionName

  case mEnhancedTerms of
    Nothing ->
      putStrLn $ "Auction " <> show auctionName <> " does not exist."
    Just eTerms -> do
      currentTime <- getCurrentTime
      currentTimeMs <- currentTimeMilliseconds
      currentStage <- currentAuctionStage (terms eTerms)
      let showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
          mSecsLeft = secondsLeftInInterval currentTimeMs (stageToInterval (terms eTerms) currentStage)
      putStrLn $ showTime currentTime
      putStrLn $ "Auction: " <> show auctionName
      putStrLn $ "Seller: " <> show (sellerActor eTerms)
      putStrLn $
        "Current stage: "
          <> show currentStage
          <> case mSecsLeft of
            Nothing -> mempty
            Just s -> "\n" <> show s <> " seconds left until next stage"

  threadDelay 0.2
  watchAuction auctionName

secondsLeftInInterval :: Integer -> Interval POSIXTime -> Maybe Integer
secondsLeftInInterval now (Interval _ (UpperBound (Finite (POSIXTime t)) inclusive)) = Just $ (t - now - if inclusive then 0 else 1) `div` 1000
secondsLeftInInterval _ _ = Nothing
