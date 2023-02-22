module CLI.Actions (
  CliInput (..),
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

-- Hydra auction CLI imports
import CLI.Config (
  AuctionName,
  CliEnhancedAuctionTerms (..),
  configToAuctionTerms,
  constructTermsDynamic,
  readAuctionTerms,
  readAuctionTermsConfig,
  readCliEnhancedAuctionTerms,
  writeAuctionTermsDynamic,
 )
import CLI.Prettyprinter (prettyPrintUtxo)

seedAmount :: Lovelace
seedAmount = 100_000_000

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

data CliInput = MkCliInput
  { cliActor :: Actor
  , cliVerbosity :: Bool
  }

handleCliAction :: CliAction -> Runner ()
handleCliAction userAction = do
  MkExecutionContext {actor} <- ask
  case userAction of
    Seed ->
      initWallet seedAmount actor
    Prepare sellerActor -> do
      forM_ allActors $ initWallet seedAmount
      void $ withActor sellerActor mintOneTestNFT
    ShowScriptUtxos auctionName script -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      utxos <- scriptUtxos script terms
      liftIO $ prettyPrintUtxo utxos
    ShowUtxos -> do
      utxos <- actorTipUtxo
      liftIO $ prettyPrintUtxo utxos
    ShowAllUtxos -> do
      forM_ allActors $ \a -> do
        utxos <- withActor a actorTipUtxo
        liftIO $ print a
        liftIO $ prettyPrintUtxo utxos
        liftIO $ putStrLn "\n"
    MintTestNFT ->
      void mintOneTestNFT
    AuctionAnounce auctionName utxo -> do
      dynamic <- liftIO $ constructTermsDynamic actor utxo
      liftIO $ writeAuctionTermsDynamic auctionName dynamic
      -- FIXME: proper error printing
      Just config <- liftIO $ readAuctionTermsConfig auctionName
      terms <- liftIO $ configToAuctionTerms config dynamic
      announceAuction terms
    StartBidding auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      startBidding terms
    NewBid auctionName bidAmount -> do
      -- FIXME: proper error printing
      Just CliEnhancedAuctionTerms {terms, sellerActor} <- liftIO $ readCliEnhancedAuctionTerms auctionName
      if actor == sellerActor
        then liftIO $ putStrLn "Seller cannot place a bid"
        else newBid terms bidAmount
    BidderBuys auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      bidderBuys terms
    SellerReclaims auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      sellerReclaims terms
    Cleanup auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      cleanupTx terms
