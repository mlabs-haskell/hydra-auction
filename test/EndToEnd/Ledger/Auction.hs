module EndToEnd.Ledger.Auction (testSuite) where

-- Prelude imports
import Hydra.Prelude (MonadIO (liftIO), SomeException, fail)
import PlutusTx.Prelude

-- Haskell imports

import Control.Monad (void)
import Control.Monad.Catch (try)

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- Hydra imports
import Hydra.Cardano.Api (mkTxIn)

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  sellerReclaims,
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

-- Hydra auction test imports
import EndToEnd.Ledger.L1Steps (createTermsWithTestNFT)
import EndToEnd.Utils (assertNFTNumEquals, assertUTxOsInScriptEquals, config, mkAssertion)

testSuite :: TestTree
testSuite =
  testGroup
    "Ledger - Auction"
    [ testCase "bidder-buys" bidderBuysTest
    , testCase "seller-reclaims" sellerReclaimsTest
    , testCase "seller-bids" sellerBidsTest
    -- FIXME: disabled until M6
    -- , testCase "unauthorised-bidder" unauthorisedBidderTest
    ]

bidderBuysTest :: Assertion
bidderBuysTest = mkAssertion $ do
  let seller = Alice
      buyer1 = Bob
      buyer2 = Carol

  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]

  terms <- createTermsWithTestNFT config nonExistentHeadIdStub

  announceAuction terms

  waitUntil $ biddingStart terms
  actorsPkh <- liftIO $ getActorsPubKeyHash [buyer1, buyer2]
  startBidding terms (ApprovedBidders actorsPkh)

  assertNFTNumEquals seller 0

  withActor buyer1 $ newBid terms $ startingBid terms
  withActor buyer2 $ newBid terms $ startingBid terms + minimumBidIncrement terms

  waitUntil $ biddingEnd terms
  withActor buyer2 $ bidderBuys terms

  assertUTxOsInScriptEquals FeeEscrow terms 1

  assertNFTNumEquals seller 0
  assertNFTNumEquals buyer1 0
  assertNFTNumEquals buyer2 1

  waitUntil $ cleanup terms
  cleanupTx terms

sellerReclaimsTest :: Assertion
sellerReclaimsTest = mkAssertion $ do
  let seller = Alice

  void $ initWallet 100_000_000 seller

  terms <- createTermsWithTestNFT config nonExistentHeadIdStub

  announceAuction terms

  waitUntil $ biddingStart terms
  startBidding terms (ApprovedBidders [])
  assertNFTNumEquals seller 0

  waitUntil $ voucherExpiry terms
  sellerReclaims terms

  assertNFTNumEquals seller 1
  assertUTxOsInScriptEquals FeeEscrow terms 1

  waitUntil $ cleanup terms
  cleanupTx terms

sellerBidsTest :: Assertion
sellerBidsTest = mkAssertion $ do
  let seller = Alice

  void $ initWallet 100_000_000 seller

  terms <- createTermsWithTestNFT config nonExistentHeadIdStub

  announceAuction terms

  waitUntil $ biddingStart terms
  sellerPkh <- liftIO $ getActorsPubKeyHash [seller]
  result <- try $ startBidding terms (ApprovedBidders sellerPkh)

  case result of
    Left (_ :: SomeException) -> return ()
    Right _ -> fail "Start bidding should fail"

{-
unauthorisedBidderTest :: Assertion
unauthorisedBidderTest = mkAssertion $ do
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

  waitUntil $ biddingStart terms
  actorsPkh <- liftIO $ getActorsPubKeyHash [buyer1]
  startBidding terms (ApprovedBidders actorsPkh)

  assertNFTNumEquals seller 0

  withActor buyer1 $ newBid terms $ startingBid terms
  result <- try $ withActor buyer2 $ newBid terms $ startingBid terms + minimumBidIncrement terms

  case result of
    Left (_ :: SomeException) -> return ()
    Right _ -> fail "New bid should fail, actor is not authorised"
-}
