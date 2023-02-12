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
import CardanoNode (withCardanoNodeDevnet)
import CliConfig (
  AuctionName,
  CLIEnhancedAuctionTerms (..),
  configToAuctionTerms,
  constructTermsDynamic,
  readAuctionTerms,
  readAuctionTermsConfig,
  readCLIEnhancedAuctionTerms,
  writeAuctionTermsDynamic,
 )
import Hydra.Logging (contramap)

import Control.Monad (forM_, void)
import Hydra.Cardano.Api (Lovelace)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (keysFor)
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

data CliAction
  = RunCardanoNode
  | ShowScriptUtxos !AuctionName !AuctionScript
  | ShowUtxos !Actor
  | Seed !Actor
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
      withCardanoNodeDevnet
        (contramap FromCardanoNode tracer)
        "."
        $ \_ ->
          error "Not implemented: RunCardanoNode"
    Seed actor ->
      initWallet actor seedAmount
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
      Just (CLIEnhancedAuctionTerms {terms, sellerActor}) <-
        liftIO $ readCLIEnhancedAuctionTerms auctionName
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
      Just (CLIEnhancedAuctionTerms {terms, sellerActor}) <-
        liftIO $ readCLIEnhancedAuctionTerms auctionName
      sellerReclaims sellerActor terms

prettyPrintUtxo :: (Foldable t, Show a) => t a -> IO ()
prettyPrintUtxo utxo = do
  putStrLn "Utxos: \n"
  -- FIXME print properly
  forM_ (toList utxo) $ \x ->
    putStrLn $ show x
