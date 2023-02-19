module CLI.Actions (
  CliInput (..),
  CliAction (..),
  handleCliAction,
  seedAmount,
) where

-- Prelude imports
import Hydra.Prelude (ask, contramap, liftIO)
import Prelude

-- Haskell imports
import Control.Monad (forM_, void)
import Data.Map.Strict qualified as Map

-- Cardano node imports
import Cardano.Api (TxIn, TxOut (..))

-- Hydra imports
import Cardano.Api.UTxO (UTxO, toMap)
import Hydra.Cardano.Api (Lovelace, TxOut, TxOutValue (..))
import Hydra.Cluster.Fixture (Actor (..))

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript)
import HydraAuction.Runner (
  ExecutionContext (..),
  HydraAuctionLog (..),
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

-- Hydra auction CLI imports
import CLI.CardanoNode (runCardanoNode)
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
  | Cleanup !AuctionName

data CliInput = MkCliInput
  { cmd :: CliAction
  , verbosity :: Bool
  }

handleCliAction :: CliAction -> Runner ()
handleCliAction userAction = do
  MkExecutionContext {tracer} <- ask
  case userAction of
    RunCardanoNode -> liftIO $ do
      putStrLn "Running cardano-node"
      runCardanoNode $ contramap FromHydra tracer
    Seed actor ->
      initWallet seedAmount actor
    Prepare sellerActor -> do
      forM_ allActors $ initWallet seedAmount
      void $ mintOneTestNFT sellerActor
    ShowScriptUtxos auctionName script -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      utxos <- scriptUtxos script terms
      liftIO $ prettyPrintUtxo utxos
    ShowUtxos actor -> do
      utxos <- actorTipUtxo actor
      liftIO $ prettyPrintUtxo utxos
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
    Cleanup auctionName -> do
      -- FIXME: proper error printing
      Just (CliEnhancedAuctionTerms {terms, sellerActor}) <-
        liftIO $ readCliEnhancedAuctionTerms auctionName
      cleanupTx sellerActor terms

prettyPrintUtxo :: UTxO -> IO ()
prettyPrintUtxo utxo = do
  putStrLn "Utxos: \n"
  -- FIXME print properly
  forM_ (Map.toList $ toMap utxo) $ \(x, y) ->
    putStrLn $ show x <> ": " <> showValueTxOut y

showValueTxOut :: Hydra.Cardano.Api.TxOut ctx -> String
showValueTxOut (Cardano.Api.TxOut _address txOutValue _datum _refScript) =
  case txOutValue of
    TxOutValue _era value -> show value
    TxOutAdaOnly _era value -> show value