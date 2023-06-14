module CLI.Printing (
  announceActionExecution,
  prettyPrintCurrentActorUtxos,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad.Trans (MonadIO (..))

-- Hydra auction imports
import HydraAuctionUtils.Monads (MonadCardanoClient)
import HydraAuctionUtils.Monads.Actors (
  MonadHasActor (..),
  actorTipUtxo,
 )
import HydraAuctionUtils.PrettyPrinting (prettyPrintUtxo)
import HydraAuctionUtils.Types.Natural (naturalToInt)

-- Hydra auction CLI imports
import CLI.Types (CliAction (..))

announceActionExecution ::
  forall m.
  (MonadIO m, MonadHasActor m, MonadCardanoClient m) =>
  CliAction ->
  m ()
announceActionExecution action = do
  currentActor <- askActor
  liftIO . putStrLn $ case action of
    Seed -> "Seeding all wallets with 100 ADA."
    Prepare sellerActor ->
      "Seeding all wallets with 100 ADA and minting the test NFT for "
        <> show sellerActor
        <> "."
    ShowScriptUtxos auctionName script ->
      "Showing all utxos under the "
        <> show script
        <> " script for auction"
        <> show auctionName
        <> "."
    ShowAllUtxos -> "Showing all utxos in everyone's wallet."
    MintTestNFT ->
      "Minting the test NFT for "
        <> show currentActor
        <> "."
    AuctionAnounce auctionName ->
      show currentActor
        <> " announces auction called "
        <> show auctionName
        <> "."
    StartBidding auctionName ->
      show currentActor
        <> " starts the bidding phase of auction "
        <> show auctionName
        <> "."
    NewBid auctionName bidAmount _ ->
      show currentActor
        <> " places a new bid of "
        <> show (naturalToInt bidAmount `div` 1_000_000)
        <> " ADA in auction "
        <> show auctionName
        <> "."
    BidderBuys {} ->
      show currentActor <> " buys the auction lot, as the winning bidder."
    BidderClaimsDeposit {} ->
      show currentActor <> " reclaims their deposit, as a losing bidder."
    SellerReclaims {} ->
      show currentActor <> " reclaims the auction lot, as the seller."
    SellerClaimsDepositFor _ bidderActor ->
      show currentActor
        <> ", as the seller, reclaims the deposit for "
        <> show bidderActor
    Cleanup auctionName ->
      "Cleaning up all remaining script utxos for auction "
        <> show auctionName
        <> "."
    _ -> ""

prettyPrintCurrentActorUtxos ::
  forall m. (MonadIO m, MonadCardanoClient m, MonadHasActor m) => m ()
prettyPrintCurrentActorUtxos = do
  actor <- askActor
  liftIO . putStrLn $
    show actor
      <> " now has the following utxos in their wallet."
  utxos <- actorTipUtxo
  liftIO $ prettyPrintUtxo utxos
