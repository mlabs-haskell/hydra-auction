module EndToEnd.Ledger (testSuite) where

-- Prelude imports
import Hydra.Prelude (MonadIO (liftIO), SomeException, fail)
import PlutusTx.Prelude

-- Haskell imports
import Control.Monad.Catch (try)
import Data.Maybe (fromJust)

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

-- Cardano node imports
import Cardano.Api.UTxO qualified as UTxO

-- Plutus imports
import Plutus.V1.Ledger.Value (assetClassValueOf)

-- Hydra imports
import Hydra.Cardano.Api (mkTxIn, toPlutusValue, txOutValue)

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.OnChain.TestNFT (testNftAssetClass)
import HydraAuction.Runner (
  Runner,
  initWallet,
  withActor,
 )
import HydraAuction.Runner.Time (waitUntil)
import HydraAuction.Tx.Common (actorTipUtxo, scriptUtxos)
import HydraAuction.Tx.Deposit (losingBidderClaimDeposit, mkDeposit, sellerClaimDepositFor)
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
import HydraAuctionUtils.Fixture (Actor (..), getActorsPubKeyHash)

-- Hydra auction test imports
import EndToEnd.Utils (mkAssertion)

testSuite :: TestTree
testSuite =
  testGroup
    "L1"
    [ testCase "bidder-buys" bidderBuysTest
    , testCase "seller-reclaims" sellerReclaimsTest
    , testCase "seller-bids" sellerBidsTest
    , testCase "unauthorised-bidder" unauthorisedBidderTest
    , testGroup "bidder-deposit" bidderDepositTests
    ]

bidderDepositTests :: [TestTree]
bidderDepositTests =
  [ testCase "losing-bidder" losingBidderClaimDepositTest
  , testCase "losing-bidder-double-claim" losingBidderDoubleClaimTest
  , testCase "seller-claims" sellerClaimsDepositTest
  , testCase "seller-claims-losing-deposit" sellerClaimsLosingDepositTest
  ]

assertNFTNumEquals :: Actor -> Integer -> Runner ()
assertNFTNumEquals actor expectedNum = do
  utxo <- withActor actor actorTipUtxo
  liftIO $ do
    let value = mconcat [toPlutusValue $ txOutValue out | (_, out) <- UTxO.pairs utxo]
    assetClassValueOf value testNftAssetClass @?= expectedNum

assertUTxOsInScriptEquals :: AuctionScript -> AuctionTerms -> Integer -> Runner ()
assertUTxOsInScriptEquals script terms expectedNum = do
  utxo <- scriptUtxos script terms
  liftIO $ length (UTxO.pairs utxo) @?= expectedNum

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
  actorsPkh <- liftIO $ getActorsPubKeyHash [buyer1, buyer2]
  startBidding terms (ApprovedBidders actorsPkh)

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
  startBidding terms (ApprovedBidders [])
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
  sellerPkh <- liftIO $ getActorsPubKeyHash [seller]
  result <- try $ startBidding terms (ApprovedBidders sellerPkh)

  case result of
    Left (_ :: SomeException) -> return ()
    Right _ -> fail "Start bidding should fail"

unauthorisedBidderTest :: Assertion
unauthorisedBidderTest = mkAssertion $ do
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
  actorsPkh <- liftIO $ getActorsPubKeyHash [buyer1]
  startBidding terms (ApprovedBidders actorsPkh)

  assertNFTNumEquals seller 0

  withActor buyer1 $ newBid terms $ startingBid terms
  result <- try $ withActor buyer2 $ newBid terms $ startingBid terms + minimumBidIncrement terms

  case result of
    Left (_ :: SomeException) -> return ()
    Right _ -> fail "New bid should fail, actor is not authorised"

losingBidderClaimDepositTest :: Assertion
losingBidderClaimDepositTest = mkAssertion $ do
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

  withActor buyer1 $ mkDeposit terms
  withActor buyer2 $ mkDeposit terms

  assertUTxOsInScriptEquals Deposit terms 2

  waitUntil $ biddingStart terms
  actorsPkh <- liftIO $ getActorsPubKeyHash [buyer1, buyer2]
  startBidding terms (ApprovedBidders actorsPkh)

  assertNFTNumEquals seller 0

  withActor buyer1 $ newBid terms $ startingBid terms
  withActor buyer2 $ newBid terms $ startingBid terms + minimumBidIncrement terms

  waitUntil $ biddingEnd terms

  withActor buyer1 $ losingBidderClaimDeposit terms

  assertUTxOsInScriptEquals Deposit terms 1

losingBidderDoubleClaimTest :: Assertion
losingBidderDoubleClaimTest = mkAssertion $ do
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

  withActor buyer1 $ mkDeposit terms
  withActor buyer2 $ mkDeposit terms

  assertUTxOsInScriptEquals Deposit terms 2

  waitUntil $ biddingStart terms
  actorsPkh <- liftIO $ getActorsPubKeyHash [buyer1, buyer2]
  startBidding terms (ApprovedBidders actorsPkh)

  assertNFTNumEquals seller 0

  withActor buyer1 $ newBid terms $ startingBid terms
  withActor buyer2 $ newBid terms $ startingBid terms + minimumBidIncrement terms

  waitUntil $ biddingEnd terms

  withActor buyer1 $ losingBidderClaimDeposit terms

  assertUTxOsInScriptEquals Deposit terms 1

  result <- try $ withActor buyer1 $ losingBidderClaimDeposit terms

  case result of
    Left (_ :: SomeException) -> assertUTxOsInScriptEquals Deposit terms 1
    Right _ -> fail "User should not be able to claim depoist twice"

sellerClaimsDepositTest :: Assertion
sellerClaimsDepositTest = mkAssertion $ do
  let seller = Alice
      buyer = Bob

  mapM_ (initWallet 100_000_000) [seller, buyer]

  nftTx <- mintOneTestNFT
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic seller utxoRef
    configToAuctionTerms config dynamicState

  assertNFTNumEquals seller 1

  announceAuction terms

  withActor buyer $ mkDeposit terms

  assertUTxOsInScriptEquals Deposit terms 1

  waitUntil $ biddingStart terms
  actorsPkh <- liftIO $ getActorsPubKeyHash [buyer]
  startBidding terms (ApprovedBidders actorsPkh)

  assertNFTNumEquals seller 0

  withActor buyer $ newBid terms $ startingBid terms

  waitUntil $ voucherExpiry terms

  sellerClaimDepositFor terms (head actorsPkh)

  assertUTxOsInScriptEquals Deposit terms 0

sellerClaimsLosingDepositTest :: Assertion
sellerClaimsLosingDepositTest = mkAssertion $ do
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

  withActor buyer1 $ mkDeposit terms
  withActor buyer2 $ mkDeposit terms

  assertUTxOsInScriptEquals Deposit terms 2

  waitUntil $ biddingStart terms
  actorsPkh <- liftIO $ getActorsPubKeyHash [buyer1, buyer2]
  startBidding terms (ApprovedBidders actorsPkh)

  assertNFTNumEquals seller 0

  withActor buyer1 $ newBid terms $ startingBid terms
  withActor buyer2 $ newBid terms $ startingBid terms + minimumBidIncrement terms

  waitUntil $ voucherExpiry terms

  result <- try $ sellerClaimDepositFor terms (head actorsPkh)

  case result of
    Left (_ :: SomeException) -> assertUTxOsInScriptEquals Deposit terms 2
    Right _ -> fail "Seller should not be able to claim deposit from losing bidder"
