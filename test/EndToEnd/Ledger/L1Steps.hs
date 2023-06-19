module EndToEnd.Ledger.L1Steps (
  createTermsWithTestNFT,
  announceAndStartBidding,
  correctBidNo,
  inititalAmount,
  performBidderBuys,
  performBidderBuysWithDeposit,
  performInit,
  natToLovelace,
  seller,
  buyer1,
  buyer2,
) where

-- Prelude

import HydraAuctionUtils.Prelude hiding ((*), (+))
import PlutusTx.Prelude ((*), (+))

-- Hydra imports

import Hydra.Cardano.Api (mkTxIn)
import Hydra.Chain (HeadId)
import Hydra.Chain.Direct.Tx (headIdToCurrencySymbol)

-- HydraAuction imports

import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Tx.Escrow (announceAuction, bidderBuys, startBidding)
import HydraAuction.Tx.TermsConfig (
  AuctionTermsConfig,
  configToAuctionTerms,
  constructTermsDynamic,
 )
import HydraAuction.Tx.TestNFT (mintOneTestNFT)
import HydraAuction.Types (
  AuctionTerms (..),
  calculateTotalFee,
 )
import HydraAuctionUtils.Extras.CardanoApi (Lovelace (..))
import HydraAuctionUtils.Fixture (Actor (..))
import HydraAuctionUtils.L1.Runner (
  L1Runner,
  initWallet,
  queryAdaWithoutFees,
  withActor,
 )
import HydraAuctionUtils.Monads (waitUntil)
import HydraAuctionUtils.Tx.Build (minLovelace)
import HydraAuctionUtils.Types.Natural (
  Natural,
  intToNatural,
  naturalToInt,
 )

-- HydraAuction test imports

import EndToEnd.Utils (
  assertAdaWithoutFeesEquals,
  assertNFTNumEquals,
  assertUTxOsInScriptEquals,
 )
import HydraAuctionUtils.Monads.Actors (MonadHasActor (..), WithActorT (..))

natToLovelace :: Natural -> Lovelace
natToLovelace = Lovelace . naturalToInt

inititalAmount :: Lovelace
inititalAmount = Lovelace 100_000_000

seller, buyer1, buyer2 :: Actor
seller = Alice
buyer1 = Bob
buyer2 = Carol

performInit :: HasCallStack => L1Runner ()
performInit = do
  withActor seller $ assertAdaWithoutFeesEquals (Lovelace 0)
  withActor buyer1 $ assertAdaWithoutFeesEquals (Lovelace 0)
  withActor buyer2 $ assertAdaWithoutFeesEquals (Lovelace 0)
  mapM_ (initWallet 100_000_000) [seller, buyer1, buyer2]
  withActor seller $ assertAdaWithoutFeesEquals inititalAmount
  withActor buyer1 $ assertAdaWithoutFeesEquals inititalAmount
  withActor buyer2 $ assertAdaWithoutFeesEquals inititalAmount

performBidderBuys ::
  HasCallStack => AuctionTerms -> Actor -> Natural -> L1Runner ()
performBidderBuys terms buyer =
  performBidderBuysWithDeposit terms buyer (Lovelace 0)

performBidderBuysWithDeposit ::
  HasCallStack => AuctionTerms -> Actor -> Lovelace -> Natural -> L1Runner ()
performBidderBuysWithDeposit terms buyer deposit finalBid = do
  waitUntil $ biddingEnd terms
  buyerAmountBefore <- withActor buyer queryAdaWithoutFees
  sellerAmountBefore <- withActor seller queryAdaWithoutFees
  let fee = Lovelace $ calculateTotalFee terms
  withActor buyer $ bidderBuys terms
  withActor seller $
    assertAdaWithoutFeesEquals $
      sellerAmountBefore + natToLovelace finalBid - fee
  -- FIXME: this minLovelace distribution is a bug
  withActor buyer $
    assertAdaWithoutFeesEquals $
      buyerAmountBefore - natToLovelace finalBid + deposit + minLovelace
  assertNFTNumEquals seller 0
  assertNFTNumEquals buyer1 0
  assertNFTNumEquals buyer2 1
  assertUTxOsInScriptEquals FeeEscrow terms 1

correctBidNo :: AuctionTerms -> Integer -> Natural
correctBidNo terms n =
  if n >= 0 && naturalToInt (startingBid terms) > 0
    then
      fromJust $
        intToNatural $
          naturalToInt (startingBid terms)
            + n * naturalToInt (minimumBidIncrement terms)
    else error "BidNo should be non-negative"

createTermsWithTestNFT ::
  AuctionTermsConfig -> HeadId -> WithActorT L1Runner AuctionTerms
createTermsWithTestNFT config headId = do
  seller' <- askActor

  utxoRef <- do
    nftTx <- mintOneTestNFT
    return $ mkTxIn nftTx 0

  dynamicState <-
    liftIO $
      constructTermsDynamic
        seller
        utxoRef
        (headIdToCurrencySymbol headId)

  lift $ assertNFTNumEquals seller' 1

  liftIO $ configToAuctionTerms config dynamicState

announceAndStartBidding :: AuctionTerms -> WithActorT L1Runner ()
announceAndStartBidding terms = do
  announceAuction terms

  lift $ waitUntil $ biddingStart terms

  startBidding terms
