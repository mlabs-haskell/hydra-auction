module EndToEnd.Scenario (scenarioSpec) where

import Prelude

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX qualified as POSIXTime
import Hydra.Cardano.Api (mkTxIn)
import Hydra.Cluster.Fixture (Actor (..))

import Test.Hydra.Prelude

import HydraAuction.Tx.Escrow
import HydraAuction.Tx.StandingBid
import HydraAuction.Tx.TermsConfig
import HydraAuction.Tx.TestNFT
import HydraAuction.Types

import EndToEnd.Common

scenarioSpec :: Spec
scenarioSpec =
  describe "End-to-end scenario test" $ do
    it "Successful bid" $ do
      runScenario $ do
        node' <- asks node

        let seller = Alice
            buyer = Bob
        initWallet seller 100_000_000
        initWallet buyer 100_000_000

        nftTx <- liftIO $ mintOneTestNFT node' seller
        let utxoRef = mkTxIn nftTx 0

        currentTimeSeconds <- liftIO $ round `fmap` POSIXTime.getPOSIXTime
        let config =
              TermsConfig
                { biddingStartPosixSeconds = fromJust $ intToNatural currentTimeSeconds
                , biddingEndDelta = fromJust $ intToNatural 3
                , voucherExpiryDelta = fromJust $ intToNatural 4
                , cleanupDelta = fromJust $ intToNatural 5
                , auctionFee = fromJust $ intToNatural 4_000_000
                , startingBid = fromJust $ intToNatural 8_000_000
                , minimumBidIncrement = fromJust $ intToNatural 8_000_000
                }
        terms <- liftIO $ termsFromConfig config seller utxoRef

        liftIO $ announceAuction node' seller terms
        liftIO $ startBidding node' seller terms

        liftIO $ newBid node' buyer terms (fromJust $ intToNatural 16_000_000)
        liftIO $ bidderBuys node' buyer terms

    it "Seller reclaims" $ do
      runScenario $ do
        node' <- asks node

        let seller = Alice
            buyer = Bob
        initWallet seller 100_000_000
        initWallet buyer 100_000_000

        nftTx <- liftIO $ mintOneTestNFT node' seller
        let utxoRef = mkTxIn nftTx 0

        currentTimeSeconds <- liftIO $ round `fmap` POSIXTime.getPOSIXTime
        let config =
              TermsConfig
                { biddingStartPosixSeconds = fromJust $ intToNatural currentTimeSeconds
                , biddingEndDelta = fromJust $ intToNatural 2
                , voucherExpiryDelta = fromJust $ intToNatural 3
                , cleanupDelta = fromJust $ intToNatural 4
                , auctionFee = fromJust $ intToNatural 4_000_000
                , startingBid = fromJust $ intToNatural 8_000_000
                , minimumBidIncrement = fromJust $ intToNatural 8_000_000
                }
        terms <- liftIO $ termsFromConfig config seller utxoRef

        liftIO $ announceAuction node' seller terms
        liftIO $ startBidding node' seller terms

        liftIO $ threadDelay $ 1 * 1_000_000

        liftIO $ sellerReclaims node' seller terms
