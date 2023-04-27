module EndToEnd.Ledger.L1Steps (
  createTermsWithTestNFT,
  announceAndStartBidding,
  correctBidNo,
) where

-- Prelude
import PlutusTx.Prelude ((*), (+))
import Prelude hiding ((*), (+))

-- Haskell imports
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)

-- Hydra imports

import Hydra.Cardano.Api (mkTxIn)
import Hydra.Chain (HeadId)
import Hydra.Chain.Direct.Tx (headIdToCurrencySymbol)

-- HydraAuction imports

import HydraAuction.Tx.Escrow (announceAuction, startBidding)
import HydraAuction.Tx.TermsConfig (
  AuctionTermsConfig,
  configToAuctionTerms,
  constructTermsDynamic,
 )
import HydraAuction.Tx.TestNFT (mintOneTestNFT)
import HydraAuction.Types (
  ApprovedBidders (..),
  AuctionTerms (..),
  Natural,
  intToNatural,
  naturalToInt,
 )
import HydraAuctionUtils.Fixture (getActorsPubKeyHash)
import HydraAuctionUtils.L1.Runner (Runner)
import HydraAuctionUtils.L1.Runner.Time (waitUntil)

-- HydraAuction test imports

import EndToEnd.Utils (assertNFTNumEquals)
import HydraAuctionUtils.Monads.Actors (MonadHasActor (..))

correctBidNo :: AuctionTerms -> Integer -> Natural
correctBidNo terms n =
  if n >= 0 && naturalToInt (startingBid terms) > 0
    then
      fromJust $
        intToNatural $
          naturalToInt (startingBid terms)
            + n * naturalToInt (minimumBidIncrement terms)
    else error "BidNo should be non-negative"

createTermsWithTestNFT :: AuctionTermsConfig -> HeadId -> Runner AuctionTerms
createTermsWithTestNFT config headId = do
  seller <- askActor

  utxoRef <- do
    nftTx <- mintOneTestNFT
    return $ mkTxIn nftTx 0

  dynamicState <-
    liftIO $
      constructTermsDynamic
        seller
        utxoRef
        (headIdToCurrencySymbol headId)

  assertNFTNumEquals seller 1

  liftIO $ configToAuctionTerms config dynamicState

announceAndStartBidding :: AuctionTerms -> Runner ()
announceAndStartBidding terms = do
  announceAuction terms

  waitUntil $ biddingStart terms

  -- FIXME: checks are disabled until M6
  actorsPkh <- liftIO $ getActorsPubKeyHash []
  startBidding terms (ApprovedBidders actorsPkh)
