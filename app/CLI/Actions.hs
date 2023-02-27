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

-- Plutus imports
import Plutus.V1.Ledger.Address (pubKeyHashAddress)

-- Hydra imports
import Hydra.Cardano.Api (Lovelace, TxIn, pattern ShelleyAddressInEra)

-- Hydra auction imports
import HydraAuction.Fixture (Actor (..))
import HydraAuction.OnChain (AuctionScript)
import HydraAuction.Runner (
  ExecutionContext (..),
  Runner,
  initWallet,
  withActor,
 )
import HydraAuction.Tx.Common (
  actorTipUtxo,
  addressAndKeys,
  currentAuctionStage,
  fromPlutusAddressInRunner,
  scriptUtxos,
 )
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  currentWinningBidder,
  sellerReclaims,
  startBidding,
 )
import HydraAuction.Tx.StandingBid (cleanupTx, newBid)
import HydraAuction.Tx.TermsConfig (constructTermsDynamic)
import HydraAuction.Tx.TestNFT (mintOneTestNFT)
import HydraAuction.Types (AuctionStage (..), AuctionTerms, Natural, naturalToInt)

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
  = ShowCurrentStage !AuctionName
  | ShowScriptUtxos !AuctionName !AuctionScript
  | ShowUtxos
  | ShowAllUtxos
  | ShowCurrentWinnigBidder !AuctionName
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

doOnMatchingStage :: AuctionTerms -> AuctionStage -> Runner () -> Runner ()
doOnMatchingStage terms requiredStage action = do
  stage <- liftIO $ currentAuctionStage terms
  if requiredStage == stage
    then action
    else
      liftIO $
        putStrLn
          ( "Wrong stage for this transaction. Now: " <> show stage
              <> ", while required: "
              <> show requiredStage
          )

handleCliAction :: CliAction -> Runner ()
handleCliAction userAction = do
  MkExecutionContext {actor} <- ask
  case userAction of
    ShowCurrentStage auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      liftIO $ do
        stage <- currentAuctionStage terms
        putStrLn $ "Current stage: " <> show stage
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
    ShowCurrentWinnigBidder auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      -- FIXME: show actor instread of PubKey
      winningBidderPk <- currentWinningBidder terms
      liftIO $ print winningBidderPk
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
          doOnMatchingStage terms BiddingStartedStage $
            newBid terms bidAmount
    BidderBuys auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      liftIO . putStrLn $
        show actor
          <> " buys the auction lot, as the winning bidder."
      doOnMatchingStage terms BiddingEndedStage $ do
        mWinningBidderPk <- currentWinningBidder terms
        (currentActorAddress, _, _) <- addressAndKeys
        case mWinningBidderPk of
          Just winningBidderPk -> do
            winningBidderAddress <-
              fromPlutusAddressInRunner $
                pubKeyHashAddress winningBidderPk
            if winningBidderAddress == ShelleyAddressInEra currentActorAddress
              then do
                bidderBuys terms
                utxos <- actorTipUtxo
                liftIO $ prettyPrintUtxo utxos
              else
                liftIO $
                  putStrLn "Cannot perform: Other actor is the winning bidder!"
          Nothing ->
            liftIO $ putStrLn "Cannot perform: No bid is placed!"
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
      doOnMatchingStage terms VoucherExpiredStage $
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
      doOnMatchingStage terms VoucherExpiredStage $
        cleanupTx terms
