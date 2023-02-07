module EndToEnd.Scenario (scenarioSpec) where

import Prelude

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (mkTxIn)
import Hydra.Cluster.Fixture (Actor (..))

import Test.Hydra.Prelude

import HydraAuction.Runner
import HydraAuction.Tx.Escrow
import HydraAuction.Tx.StandingBid
import HydraAuction.Tx.TestNFT
import HydraAuction.Types

scenarioSpec :: Spec
scenarioSpec =
  describe "End-to-end runner tests" $ do
    it "Successful bid" $ do
      stateDir <- defStateDirectory
      executeRunner stateDir $ do
        node' <- asks node

        let seller = Alice
            buyer = Bob
        initWallet seller 100_000_000
        initWallet buyer 100_000_000

        nftTx <- liftIO $ mintOneTestNFT node' seller
        let utxoRef = mkTxIn nftTx 0
        terms <- liftIO $ constructTerms node' seller utxoRef

        liftIO $ announceAuction node' seller terms
        liftIO $ startBidding node' seller terms

        liftIO $
          newBid
            node'
            buyer
            terms
            (fromJust $ intToNatural 16_000_000)
        liftIO $ bidderBuys node' buyer terms

    it "Seller reclaims" $ do
      stateDir <- defStateDirectory
      executeRunner stateDir $ do
        node' <- asks node

        let seller = Alice
            buyer = Bob
        initWallet seller 100_000_000
        initWallet buyer 100_000_000

        nftTx <- liftIO $ mintOneTestNFT node' seller
        let utxoRef = mkTxIn nftTx 0
        terms <- liftIO $ constructTerms node' seller utxoRef

        liftIO $ announceAuction node' seller terms
        liftIO $ startBidding node' seller terms

        liftIO $ sellerReclaims node' seller terms
