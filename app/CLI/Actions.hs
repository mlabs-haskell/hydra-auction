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

seedAmount :: Lovelace
seedAmount = 100_000_000

allActors :: [Actor]
allActors = [Alice, Bob, Carol]

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

data CliInput = MkCliInput
  { ciActor :: Actor
  , ciVerbosity :: Bool
  }

handleCliAction :: CliAction -> Runner ()
handleCliAction userAction = do
  MkExecutionContext {tracer} <- ask
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
      forM_ allActors $ \_ -> do
        utxos <- actorTipUtxo
        liftIO $ prettyPrintUtxo utxos
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
      Just (CliEnhancedAuctionTerms {terms, sellerActor}) <-
        liftIO $ readCliEnhancedAuctionTerms auctionName
      withActor sellerActor $ startBidding terms
    NewBid auctionName bidAmount -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      newBid terms bidAmount
    BidderBuys auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      bidderBuys terms
    SellerReclaims auctionName -> do
      -- FIXME: proper error printing
      Just (CliEnhancedAuctionTerms {terms}) <-
        liftIO $ readCliEnhancedAuctionTerms auctionName
      sellerReclaims terms
    Cleanup auctionName -> do
      -- FIXME: proper error printing
      Just (CliEnhancedAuctionTerms {terms}) <-
        liftIO $ readCliEnhancedAuctionTerms auctionName
      cleanupTx terms

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
