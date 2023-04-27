module EndToEnd.Ledger.BidDeposit (testSuite) where

-- Prelude imports
import Hydra.Prelude (MonadIO (liftIO), SomeException, fail)
import PlutusTx.Prelude

-- Haskell imports
import Control.Monad.Catch (try)
import Data.Maybe (fromJust)

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- Hydra imports
import Hydra.Cardano.Api (mkTxIn)

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Tx.Deposit (cleanupDeposit, losingBidderClaimDeposit, mkDeposit, sellerClaimDepositFor)
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  startBidding,
 )
import HydraAuction.Tx.StandingBid (cleanupTx, newBid)
import HydraAuction.Tx.TermsConfig (
  configToAuctionTerms,
  constructTermsDynamic,
  nonExistentHeadIdStub,
 )
import HydraAuction.Tx.TestNFT (mintOneTestNFT)
import HydraAuction.Types (ApprovedBidders (..), AuctionTerms (..))
import HydraAuctionUtils.Fixture (Actor (..), getActorsPubKeyHash)
import HydraAuctionUtils.L1.Runner (
  initWallet,
  withActor,
 )
import HydraAuctionUtils.L1.Runner.Time (waitUntil)
import HydraAuctionUtils.Types.Natural (Natural, intToNatural)

-- Hydra auction test imports
import EndToEnd.Utils (assertNFTNumEquals, assertUTxOsInScriptEquals, config, mkAssertion)

testSuite :: TestTree
testSuite =
  testGroup
    "Ledger - BidDeposit"
    [ testCase "losing-bidder" losingBidderClaimDepositTest
    , testCase "losing-bidder-double-claim" losingBidderDoubleClaimTest
    , testCase "seller-claims" sellerClaimsDepositTest
    , testCase "seller-claims-losing-deposit" sellerClaimsLosingDepositTest
    , testCase "bidder-buys-with-deposit" bidderBuysWithDepositTest
    , testCase "cleanup-deposit" cleanupDepositTest
    ]

depositAmount :: Natural
depositAmount = fromJust $ intToNatural 20_000_000

losingBidderClaimDepositTest :: Assertion
losingBidderClaimDepositTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

  nftTx <- mintOneTestNFT
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic seller utxoRef nonExistentHeadIdStub
    configToAuctionTerms config dynamicState

  assertNFTNumEquals seller 1

  announceAuction terms

  withActor buyer1 $ mkDeposit terms depositAmount
  withActor buyer2 $ mkDeposit terms depositAmount

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
    dynamicState <- constructTermsDynamic seller utxoRef nonExistentHeadIdStub
    configToAuctionTerms config dynamicState

  assertNFTNumEquals seller 1

  announceAuction terms

  withActor buyer1 $ mkDeposit terms depositAmount
  withActor buyer2 $ mkDeposit terms depositAmount

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
    dynamicState <- constructTermsDynamic seller utxoRef nonExistentHeadIdStub
    configToAuctionTerms config dynamicState

  assertNFTNumEquals seller 1

  announceAuction terms

  withActor buyer $ mkDeposit terms depositAmount

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
    dynamicState <- constructTermsDynamic seller utxoRef nonExistentHeadIdStub
    configToAuctionTerms config dynamicState

  assertNFTNumEquals seller 1

  announceAuction terms

  withActor buyer1 $ mkDeposit terms depositAmount
  withActor buyer2 $ mkDeposit terms depositAmount

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

bidderBuysWithDepositTest :: Assertion
bidderBuysWithDepositTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

  nftTx <- mintOneTestNFT
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic seller utxoRef nonExistentHeadIdStub
    configToAuctionTerms config dynamicState

  assertNFTNumEquals seller 1

  announceAuction terms

  withActor buyer1 $ mkDeposit terms depositAmount
  withActor buyer2 $ mkDeposit terms depositAmount

  assertUTxOsInScriptEquals Deposit terms 2

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
  assertUTxOsInScriptEquals Deposit terms 1

  waitUntil $ cleanup terms
  cleanupTx terms

cleanupDepositTest :: Assertion
cleanupDepositTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

  nftTx <- mintOneTestNFT
  let utxoRef = mkTxIn nftTx 0

  terms <- liftIO $ do
    dynamicState <- constructTermsDynamic seller utxoRef nonExistentHeadIdStub
    configToAuctionTerms config dynamicState

  assertNFTNumEquals seller 1

  announceAuction terms

  withActor buyer1 $ mkDeposit terms depositAmount
  withActor buyer2 $ mkDeposit terms depositAmount

  assertUTxOsInScriptEquals Deposit terms 2

  waitUntil $ biddingStart terms
  actorsPkh <- liftIO $ getActorsPubKeyHash [buyer1, buyer2]
  startBidding terms (ApprovedBidders actorsPkh)

  assertNFTNumEquals seller 0

  withActor buyer1 $ newBid terms $ startingBid terms
  withActor buyer2 $ newBid terms $ startingBid terms + minimumBidIncrement terms

  waitUntil $ cleanup terms
  cleanupTx terms

  withActor buyer1 $ cleanupDeposit terms
  assertUTxOsInScriptEquals Deposit terms 1

  withActor buyer2 $ cleanupDeposit terms
  assertUTxOsInScriptEquals Deposit terms 0
