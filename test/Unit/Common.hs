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
import Test.Tasty.HUnit (Assertion, testCase, (@=?))

-- Hydra
import Hydra.Cardano.Api (TxIn)
import Hydra.Ledger.Cardano ()

-- Arbitrary instances

-- Hydra auction imports
import HydraAuction.Fixture (Actor (..))
import HydraAuction.Tx.Common (currentAuctionStage)
import HydraAuction.Tx.TermsConfig (
  AuctionTermsConfig (..),
  configToAuctionTerms,
  constructTermsDynamic,
 )
import HydraAuction.Types (AuctionStage (..), intToNatural)

testSuite :: TestTree
testSuite =
  testGroup
    "Unit-Common"
    [testCase "current-auction-stage" testCurrentAuctionStage]

testCurrentAuctionStage :: Assertion
testCurrentAuctionStage = do
  -- Using minutes, cuz TimeMachine does not support seconds
  -- Using actual time, cuz TimeMachine does not support nesting mocks
  -- Could just use absolute time mocks instead

  let config =
        AuctionTermsConfig
          { configDiffBiddingStart = 1 * 60 - 1
          , configDiffBiddingEnd = 2 * 60 - 1
          , configDiffVoucherExpiry = 3 * 60 - 1
          , configDiffCleanup = 4 * 60 - 1
          , configAuctionFeePerDelegate = fromJust $ intToNatural 4_000_000
          , configStartingBid = fromJust $ intToNatural 8_000_000
          , configMinimumBidIncrement = fromJust $ intToNatural 8_000_000
          }

  nonce <- generate arbitrary :: Prelude.IO TxIn

  -- TODO: halt
  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic Alice nonce
    configToAuctionTerms config dynamicState

  assertStageAtTime terms (0 `minutes` later) AnnouncedStage
  assertStageAtTime terms (1 `minutes` later) BiddingStartedStage
  assertStageAtTime terms (2 `minutes` later) BiddingEndedStage
  assertStageAtTime terms (3 `minutes` later) VoucherExpiredStage
  where
    assertStageAtTime terms timeDiff expectedStage = do
      _ <- travelTo timeDiff $ currentAuctionStage terms
      liftIO $ expectedStage @=? expectedStage
