module EndToEnd.CLI (testSuite) where

-- Prelude imports
import Hydra.Prelude (MonadIO (liftIO), SomeException, fail)
import PlutusTx.Prelude

-- Haskell imports

import Control.Monad (void)
import Control.Monad.Catch (try)
import Data.Maybe (fromJust)

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@=?))

-- Cardano node imports
import Cardano.Api.UTxO qualified as UTxO

-- Plutus imports
import Plutus.V1.Ledger.Value (assetClassValueOf)

-- Hydra imports
import Hydra.Cardano.Api (mkTxIn, toPlutusValue, txOutValue)

-- Hydra auction imports
import HydraAuction.OnChain.TestNFT (testNftAssetClass)
import HydraAuction.Tx.Common (actorTipUtxo)
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  sellerReclaims,
  startBidding,
 )
import HydraAuction.Tx.StandingBid (cleanupTx, newBid)
import HydraAuction.Tx.TermsConfig (
  AuctionTermsConfig (
    AuctionTermsConfig,
    configAuctionFeePerDelegate,
    configDiffBiddingEnd,
    configDiffBiddingStart,
    configDiffCleanup,
    configDiffVoucherExpiry,
    configMinimumBidIncrement,
    configStartingBid
  ),
  configToAuctionTerms,
  constructTermsDynamic,
 )
import HydraAuction.Tx.TestNFT (mintOneTestNFT)
import HydraAuction.Types (ApprovedBidders (..), AuctionTerms (..), intToNatural)
import HydraAuctionUtils.Fixture (Actor (..), getActorsPubKey)
import HydraAuctionUtils.L1.Runner (
  L1Runner,
  initWallet,
  withActor,
 )
import HydraAuctionUtils.L1.Runner.Time (waitUntil)

-- Hydra auction test imports
import EndToEnd.Utils (mkAssertion)

testSuite :: TestTree
testSuite =
  testGroup
    "L1 - CLI"
    [testCase "bidder-buys" bidderBuysTest]

assertNFTNumEquals :: Actor -> Integer -> L1Runner ()
assertNFTNumEquals actor expectedNum = do
  utxo <- withActor actor actorTipUtxo
  liftIO $ do
    let value = mconcat [toPlutusValue $ txOutValue out | (_, out) <- UTxO.pairs utxo]
    assetClassValueOf value testNftAssetClass @=? expectedNum

config :: AuctionTermsConfig
config =
  AuctionTermsConfig
    { configDiffBiddingStart = 2
    , configDiffBiddingEnd = 5
    , configDiffVoucherExpiry = 8
    , configDiffCleanup = 10
    , configAuctionFeePerDelegate = fromJust $ intToNatural 4_000_000
    , configStartingBid = fromJust $ intToNatural 8_000_000
    , configMinimumBidIncrement = fromJust $ intToNatural 8_000_000
    }

bidderBuysTest :: Assertion
bidderBuysTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  handleCliAction $ Prepare seller

  assertNFTNumEquals seller 1

  handleCliAction $ AuctionAnounce "demo"

  waitUntil $ biddingStart terms

  handleCliAction $ StartBidding "demo" [buyer1, buyer2]

  assertNFTNumEquals seller 0

  withActor buyer1 $ handleCliAction $ NewBid "demo" $ startingBid terms
  withActor buyer2 $ handleCliAction $ NewBid "demo" $ startingBid terms + minimumBidIncrement terms

  waitUntil $ biddingEnd terms

  withActor buyer2 $ handleCliAction "demo"

  assertNFTNumEquals seller 0
  assertNFTNumEquals buyer1 0
  assertNFTNumEquals buyer2 1

  waitUntil $ cleanup terms
  handleCliAction $ Cleanup "demo"
