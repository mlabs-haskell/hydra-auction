module EndToEnd.Ledger (testSuite) where

-- Prelude imports
import Hydra.Prelude (MonadIO (liftIO), SomeException, fail)
import PlutusTx.Prelude

-- Haskell imports
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
import HydraAuction.Runner (
  Runner,
  initWallet,
  withActor,
 )
import HydraAuction.Runner.Time (waitUntil)
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
import HydraAuction.Types (AuctionTerms (..), intToNatural)
import HydraAuctionUtils.Fixture (Actor (..))

-- Hydra auction test imports
import EndToEnd.Utils (mkAssertion)

testSuite :: TestTree
testSuite =
  testGroup
    "L1"
    [ testCase "bidder-buys" bidderBuysTest
    , testCase "seller-reclaims" sellerReclaimsTest
    , testCase "seller-bids" sellerBidsTest
    ]

assertNFTNumEquals :: Actor -> Integer -> Runner ()
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

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

  nftTx <- mintOneTestNFT
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic seller utxoRef
    configToAuctionTerms config dynamicState

  assertNFTNumEquals seller 1

  announceAuction terms

  waitUntil $ biddingStart terms
  startBidding terms

  assertNFTNumEquals seller 0

  withActor buyer1 $ newBid terms $ startingBid terms
  withActor buyer2 $ newBid terms $ startingBid terms + minimumBidIncrement terms

  waitUntil $ biddingEnd terms
  withActor buyer2 $ bidderBuys terms

  assertNFTNumEquals seller 0
  assertNFTNumEquals buyer1 0
  assertNFTNumEquals buyer2 1

  waitUntil $ cleanup terms
  cleanupTx terms

sellerReclaimsTest :: Assertion
sellerReclaimsTest = mkAssertion $ do
  let seller = Alice

  _ <- initWallet 100_000_000 seller

  nftTx <- mintOneTestNFT
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic seller utxoRef
    configToAuctionTerms config dynamicState

  assertNFTNumEquals seller 1
  announceAuction terms

  waitUntil $ biddingStart terms
  startBidding terms
  assertNFTNumEquals seller 0

  waitUntil $ voucherExpiry terms
  sellerReclaims terms

  assertNFTNumEquals seller 1

  waitUntil $ cleanup terms
  cleanupTx terms

sellerBidsTest :: Assertion
sellerBidsTest = mkAssertion $ do
  let seller = Alice

  _ <- initWallet 100_000_000 seller

  nftTx <- mintOneTestNFT
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic seller utxoRef
    configToAuctionTerms config dynamicState

  assertNFTNumEquals seller 1
  announceAuction terms

  waitUntil $ biddingStart terms
  startBidding terms
  assertNFTNumEquals seller 0

  result <- try $ newBid terms $ startingBid terms

  case result of
    Left (_ :: SomeException) -> return ()
    Right _ -> fail "New bid should fail"
