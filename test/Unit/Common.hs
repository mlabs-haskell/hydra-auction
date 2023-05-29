module Unit.Common (testSuite) where

-- Prelude imports
import Hydra.Prelude (MonadIO (liftIO))
import PlutusTx.Prelude
import Prelude qualified

-- Haskell imports

import Control.Monad.TimeMachine (travelTo)
import Control.Monad.TimeMachine.Cockpit (later, minutes)
import Data.Maybe (fromJust)

-- Haskell test imports

import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (generate)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

-- Hydra
import Hydra.Cardano.Api (TxIn)
import Hydra.Chain.Direct.Tx (headIdToCurrencySymbol)
import Hydra.Ledger.Cardano ()

-- Plutus imports
import PlutusLedgerApi.V1.Interval (from, interval, to)

-- Hydra auction imports
import HydraAuction.OnChain.Common (secondsLeftInInterval)
import HydraAuction.Tx.Common (currentAuctionStage)
import HydraAuction.Tx.TermsConfig (
  AuctionTermsConfig (..),
  configToAuctionTerms,
  constructTermsDynamic,
  nonExistentHeadIdStub,
 )
import HydraAuction.Types (AuctionStage (..))
import HydraAuctionUtils.Fixture (Actor (..))
import HydraAuctionUtils.Types.Natural (intToNatural)

testSuite :: TestTree
testSuite =
  testGroup
    "Unit-Common"
    [ testCase "current-auction-stage" testCurrentAuctionStage
    , testCase "seconds-left-interval" testSecondsLeftInterval
    ]

testCurrentAuctionStage :: Assertion
testCurrentAuctionStage = do
  -- Using minutes, cuz TimeMachine does not support seconds
  -- Using actual time, cuz TimeMachine does not support nesting mocks
  -- Could just use absolute time mocks instead

  -- Since TimeMachine gives us only second granularity,
  -- we define each stage to be 1 minute long, with
  -- bidding starting 30 seconds after the auction announcement
  let config =
        AuctionTermsConfig
          { configDiffBiddingStart = 30
          , configDiffBiddingEnd = 90
          , configDiffVoucherExpiry = 150
          , configDiffCleanup = 210
          , configAuctionFeePerDelegate = fromJust $ intToNatural 4_000_000
          , configStartingBid = fromJust $ intToNatural 8_000_000
          , configMinimumBidIncrement = fromJust $ intToNatural 8_000_000
          }

  nonce <- generate arbitrary :: Prelude.IO TxIn

  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic Alice nonce (headIdToCurrencySymbol nonExistentHeadIdStub)
    configToAuctionTerms config dynamicState

  assertStageAtTime terms (0 `minutes` later) AnnouncedStage
  assertStageAtTime terms (1 `minutes` later) BiddingStartedStage
  assertStageAtTime terms (2 `minutes` later) BiddingEndedStage
  assertStageAtTime terms (3 `minutes` later) VoucherExpiredStage
  assertStageAtTime terms (4 `minutes` later) CleanupStage
  where
    assertStageAtTime terms timeDiff expectedStage = do
      stage <- travelTo timeDiff $ currentAuctionStage terms
      liftIO $ stage @?= expectedStage

testSecondsLeftInterval :: Assertion
testSecondsLeftInterval = do
  let now = 1000
      interval1 = to 5000
      interval2 = interval 5000 15000
      interval3 = from 15000
      interval4 = interval 0 100

  secondsLeftInInterval now interval1 @?= Just 4
  secondsLeftInInterval now interval2 @?= Just 14
  secondsLeftInInterval now interval3 @?= Nothing
  secondsLeftInInterval now interval4 @?= Nothing
