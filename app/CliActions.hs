{-# LANGUAGE RecordWildCards #-}

module CliActions (
  CliInput (..),
  CliAction (..),
  handleCliAction,
  seedAmount,
) where

import Hydra.Prelude (ask, liftIO, toList)
import Prelude

import Cardano.Api (TxIn)
import CliConfig (
  AuctionName,
  CliEnhancedAuctionTerms (..),
  configToAuctionTerms,
  constructTermsDynamic,
  readAuctionTerms,
  readAuctionTermsConfig,
  readCliEnhancedAuctionTerms,
  writeAuctionTermsDynamic,
 )
import Hydra.Logging (contramap)

import CardanoNodeDevnet (runCardanoNode)
import Control.Monad (forM_, void)
import Hydra.Cardano.Api (Lovelace)
import Hydra.Cluster.Fixture (Actor (..))
import HydraAuction.OnChain (AuctionScript)
import HydraAuction.Runner (
  ExecutionContext (MkExecutionContext, node, tracer, verbose),
  Runner,
  initWallet,
 )
import HydraAuction.Tx.Common
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  sellerReclaims,
  startBidding,
 )
import HydraAuction.Tx.StandingBid
import HydraAuction.Tx.TestNFT
import HydraAuction.Types (Natural)
import HydraNode (
  EndToEndLog (
    FromCardanoNode
  ),
 )

seedAmount :: Lovelace
seedAmount = 100_000_000

allActors :: [Actor]
allActors = [Alice, Bob, Carol]

data CliAction
  = RunCardanoNode
  | ShowScriptUtxos !AuctionName !AuctionScript
  | ShowUtxos !Actor
  | Seed !Actor
  | Prepare !Actor
  | MintTestNFT !Actor
  | AuctionAnounce !AuctionName !Actor !TxIn
  | StartBidding !AuctionName
  | NewBid !AuctionName !Actor !Natural
  | BidderBuys !AuctionName !Actor
  | SellerReclaims !AuctionName

data CliInput = MkCliInput
  { cmd :: CliAction
  , verbosity :: Bool
  }

handleCliAction :: CliAction -> Runner ()
handleCliAction userAction = do
  MkExecutionContext {..} <- ask
  case userAction of
    RunCardanoNode -> liftIO $ do
      putStrLn "Running cardano-node"
      runCardanoNode tracer
    Seed actor ->
      initWallet actor seedAmount
    Prepare sellerActor -> do
      forM_ allActors $ \actor -> initWallet actor seedAmount
      void $ mintOneTestNFT sellerActor
    ShowScriptUtxos auctionName script -> liftIO $ do
      -- FIXME: proper error printing
      Just terms <- readAuctionTerms auctionName
      utxos <- scriptUtxos node script terms
      prettyPrintUtxo utxos
    ShowUtxos actor -> liftIO $ do
      utxos <- actorTipUtxo node actor
      prettyPrintUtxo utxos
    MintTestNFT actor ->
      void $ mintOneTestNFT actor
    AuctionAnounce auctionName sellerActor utxo -> do
      dynamic <- liftIO $ constructTermsDynamic sellerActor utxo
      liftIO $ writeAuctionTermsDynamic auctionName dynamic
      -- FIXME: proper error printing
      Just config <- liftIO $ readAuctionTermsConfig auctionName
      terms <- liftIO $ configToAuctionTerms config dynamic
      announceAuction sellerActor terms
    StartBidding auctionName -> do
      -- FIXME: proper error printing
      Just (CliEnhancedAuctionTerms {terms, sellerActor}) <-
        liftIO $ readCliEnhancedAuctionTerms auctionName
      startBidding sellerActor terms
    NewBid auctionName bidder bidAmount -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      newBid bidder terms bidAmount
    BidderBuys auctionName actor -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      bidderBuys actor terms
    SellerReclaims auctionName -> do
      -- FIXME: proper error printing
      Just (CliEnhancedAuctionTerms {terms, sellerActor}) <-
        liftIO $ readCliEnhancedAuctionTerms auctionName
      sellerReclaims sellerActor terms

prettyPrintUtxo :: (Foldable t, Show a) => t a -> IO ()
prettyPrintUtxo utxo = do
  putStrLn "Utxos: \n"
  -- FIXME print properly
  forM_ (toList utxo) $ \x ->
    putStrLn $ show x
