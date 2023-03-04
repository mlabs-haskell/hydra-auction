module CLI.Actions (
  CliAction (..),
  handleCliAction,
  seedAmount,
) where

-- Prelude imports
import Hydra.Prelude (ask, liftIO)
import Prelude

-- Haskell imports
import Control.Monad (forM_, void)

-- Hydra imports
import Hydra.Cardano.Api (Lovelace, TxIn)

-- Hydra auction imports
import HydraAuction.Fixture (Actor (..))
import HydraAuction.OnChain (AuctionScript)
import HydraAuction.Runner (
  ExecutionContext (..),
  Runner,
  initWallet,
  withActor,
 )
import HydraAuction.Tx.Common (actorTipUtxo, scriptUtxos)
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  sellerReclaims,
  startBidding,
 )
import HydraAuction.Tx.StandingBid (cleanupTx, newBid)
import HydraAuction.Tx.TermsConfig (constructTermsDynamic)
import HydraAuction.Tx.TestNFT (mintOneTestNFT)
import HydraAuction.Types (Natural, naturalToInt)

-- Hydra auction CLI imports
import CLI.Config (
  AuctionName,
  CliEnhancedAuctionTerms (..),
  configToAuctionTerms,
  readAuctionTerms,
  readAuctionTermsConfig,
  readCliEnhancedAuctionTerms,
  writeAuctionTermsDynamic,
 )
import CLI.Prettyprinter (prettyPrintUtxo)

seedAmount :: Lovelace
seedAmount = 10_000_000_000

allActors :: [Actor]
allActors = [a | a <- [minBound .. maxBound], a /= Faucet]

data CliAction
  = ShowScriptUtxos !AuctionName !AuctionScript
  | ShowUtxos
  | ShowAllUtxos
  | Seed
  | Prepare !Actor
  | MintTestNFT
  | AuctionAnounce !AuctionName !TxIn
  | StartBidding !AuctionName
  | NewBid !AuctionName !Natural
  | BidderBuys !AuctionName
  | SellerReclaims !AuctionName
  | Cleanup !AuctionName
  deriving stock (Show)

handleCliAction :: CliAction -> Runner ()
handleCliAction userAction = do
  MkExecutionContext {actor} <- ask
  case userAction of
    Seed -> do
      liftIO . putStrLn $
        "Seeding all wallets with 10,000 ADA."
      initWallet seedAmount actor
    Prepare sellerActor -> do
      liftIO . putStrLn $
        "Seeding all wallets with 10,000 ADA and minting the test NFT for "
          <> show sellerActor
          <> "."
      forM_ allActors $ initWallet seedAmount
      void $ withActor sellerActor mintOneTestNFT
      liftIO . putStrLn $
        show sellerActor
          <> " now has the following utxos in their wallet."
      utxos <- actorTipUtxo
      liftIO $ prettyPrintUtxo utxos
    ShowScriptUtxos auctionName script -> do
      liftIO . putStrLn $
        "Showing all utxos under the "
          <> show script
          <> " script for auction"
          <> show auctionName
          <> "."
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      utxos <- scriptUtxos script terms
      liftIO $ prettyPrintUtxo utxos
    ShowUtxos -> do
      liftIO . putStrLn $
        "Showing all utxos in "
          <> show actor
          <> "'s wallet."
      utxos <- actorTipUtxo
      liftIO $ prettyPrintUtxo utxos
    ShowAllUtxos -> do
      liftIO . putStrLn $
        "Showing all utxos in everyone's wallet."
      forM_ allActors $ \a -> do
        utxos <- withActor a actorTipUtxo
        liftIO $ print a
        liftIO $ prettyPrintUtxo utxos
        liftIO $ putStrLn "\n"
    MintTestNFT -> do
      liftIO . putStrLn $
        "Minting the test NFT for "
          <> show actor
          <> "."
      void mintOneTestNFT
    AuctionAnounce auctionName utxo -> do
      dynamic <- liftIO $ constructTermsDynamic actor utxo
      liftIO $ writeAuctionTermsDynamic auctionName dynamic
      -- FIXME: proper error printing
      Just config <- liftIO $ readAuctionTermsConfig auctionName
      terms <- liftIO $ configToAuctionTerms config dynamic
      liftIO . putStrLn $
        show actor
          <> " announces auction called "
          <> show auctionName
          <> "."
      announceAuction terms
    StartBidding auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      liftIO . putStrLn $
        show actor
          <> " starts the bidding phase of auction "
          <> show auctionName
          <> "."
      startBidding terms
    NewBid auctionName bidAmount -> do
      -- FIXME: proper error printing
      Just CliEnhancedAuctionTerms {terms, sellerActor} <- liftIO $ readCliEnhancedAuctionTerms auctionName
      if actor == sellerActor
        then liftIO $ putStrLn "Seller cannot place a bid"
        else do
          liftIO . putStrLn $
            show actor
              <> " places a new bid of "
              <> show (naturalToInt bidAmount `div` 1_000_000)
              <> " ADA in auction "
              <> show auctionName
              <> "."
          newBid terms bidAmount
    BidderBuys auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      liftIO . putStrLn $
        show actor
          <> " buys the auction lot, as the winning bidder."
      bidderBuys terms
      liftIO . putStrLn $
        show actor
          <> " now has the following utxos in their wallet."
      utxos <- actorTipUtxo
      liftIO $ prettyPrintUtxo utxos
    SellerReclaims auctionName -> do
      -- FIXME: proper error printing
      liftIO . putStrLn $
        show actor
          <> " reclaims the auction lot, as the seller."
      Just terms <- liftIO $ readAuctionTerms auctionName
      sellerReclaims terms
      liftIO . putStrLn $
        show actor
          <> " now has the following utxos in their wallet."
      utxos <- actorTipUtxo
      liftIO $ prettyPrintUtxo utxos
    Cleanup auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      liftIO . putStrLn $
        "Cleaning up all remaining script utxos for auction "
          <> show auctionName
          <> "."
      cleanupTx terms
