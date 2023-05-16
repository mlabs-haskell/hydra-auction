module CLI.Actions (
  CliAction (..),
  Layer (..),
  handleCliAction,
  seedAmount,
  auctionTermsFor,
) where

-- Prelude imports
import Hydra.Prelude (MonadIO, ask, liftIO)
import Prelude

-- Haskell imports
import Control.Monad (forM_, void)
import Data.IORef (IORef, readIORef)

-- Plutus imports
import Plutus.V1.Ledger.Address (pubKeyHashAddress)

-- Hydra imports

import Hydra.Cardano.Api (Lovelace, pattern ShelleyAddressInEra)
import Hydra.Chain.Direct.Tx (headIdToCurrencySymbol)

-- Cardano node imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra auction imports

import HydraAuction.Delegate.Interface (DelegateState (..))
import HydraAuction.Delegate.Interface qualified as DelegateInterface
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Tx.Common (
  currentAuctionStage,
  scriptSingleUtxo,
  scriptUtxos,
 )
import HydraAuction.Tx.Deposit (
  filterDepositGreaterThan,
  losingBidderClaimDeposit,
  mkDeposit,
  parseBidDepositDatum,
  sellerClaimDepositFor,
 )
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  sellerReclaims,
  startBidding,
 )
import HydraAuction.Tx.StandingBid (cleanupTx, createStandingBidDatum, currentWinningBidder, newBid, sellerSignatureForActor)
import HydraAuction.Tx.TermsConfig (constructTermsDynamic)
import HydraAuction.Tx.TestNFT (findTestNFT, mintOneTestNFT)
import HydraAuction.Types (
  AuctionStage (..),
  AuctionTerms,
  BidDepositDatum (..),
 )
import HydraAuctionUtils.Fixture (Actor (..), actorFromPkh, allActors, getActorPubKeyHash)
import HydraAuctionUtils.L1.Runner (
  ExecutionContext (..),
  L1Runner,
  initWallet,
  withActor,
 )
import HydraAuctionUtils.Monads (fromPlutusAddressInMonad)
import HydraAuctionUtils.Monads.Actors (
  actorTipUtxo,
  addressAndKeys,
 )
import HydraAuctionUtils.PrettyPrinting (prettyPrintUtxo)
import HydraAuctionUtils.Types.Natural (Natural, naturalToInt)

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

seedAmount :: Lovelace
seedAmount = 10_000_000_000

data Layer = L1 | L2 deriving stock (Show)

data CliAction
  = ShowCurrentStage !AuctionName
  | ShowScriptUtxos !AuctionName !AuctionScript
  | ShowUtxos
  | ShowAllUtxos
  | ShowCurrentWinningBidder !AuctionName
  | ShowActorsMinDeposit !AuctionName !Natural
  | Seed
  | Prepare !Actor
  | MintTestNFT
  | AuctionAnounce !AuctionName
  | MakeDeposit !AuctionName !Natural
  | StartBidding !AuctionName
  | MoveToL2 !AuctionName
  | NewBid !AuctionName !Natural !Layer
  | BidderBuys !AuctionName
  | BidderClaimsDeposit !AuctionName
  | SellerReclaims !AuctionName
  | SellerClaimsDepositFor !AuctionName !Actor
  | Cleanup !AuctionName
  deriving stock (Show)

doOnMatchingStage :: AuctionTerms -> AuctionStage -> L1Runner () -> L1Runner ()
doOnMatchingStage terms requiredStage action = do
  stage <- liftIO $ currentAuctionStage terms
  if requiredStage == stage
    then action
    else
      liftIO $
        putStrLn
          ( "Wrong stage for this transaction. Now: "
              <> show stage
              <> ", while required: "
              <> show requiredStage
          )

handleCliAction ::
  (DelegateInterface.FrontendRequest -> IO ()) ->
  IORef DelegateState ->
  CliAction ->
  L1Runner ()
handleCliAction sendRequestToDelegate currentDelegateStateRef userAction = do
  -- Await for initialized DelegateState
  MkExecutionContext {actor} <- ask
  case userAction of
    ShowCurrentStage auctionName -> do
      terms <- auctionTermsFor auctionName
      liftIO $ do
        stage <- currentAuctionStage terms
        putStrLn $ "Current stage: " <> show stage
    Seed -> do
      liftIO . putStrLn $
        "Seeding all wallets with 10,000 ADA."
      void $ initWallet seedAmount actor
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
      terms <- auctionTermsFor auctionName
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
    ShowCurrentWinningBidder auctionName -> do
      terms <- auctionTermsFor auctionName
      winningActor <- do
        winningBidder <- currentWinningBidder terms
        liftIO $ sequence $ actorFromPkh <$> winningBidder
      liftIO $ print winningActor
    ShowActorsMinDeposit auctionName minDeposit -> do
      terms <- auctionTermsFor auctionName
      allDeposits <- scriptUtxos Deposit terms

      let matchingDatums = (parseBidDepositDatum . snd <$>) . UTxO.pairs $ filterDepositGreaterThan minDeposit allDeposits
      actors <- liftIO $ mapM (actorFromPkh . bidDepositBidder) matchingDatums

      liftIO . putStrLn $
        "Showing actors that satisfy min deposit: "
          <> show actors
    MintTestNFT -> do
      liftIO . putStrLn $
        "Minting the test NFT for "
          <> show actor
          <> "."
      void mintOneTestNFT
    AuctionAnounce auctionName -> do
      mTxIn <- findTestNFT <$> actorTipUtxo
      case mTxIn of
        Just txIn -> do
          delegateState <- liftIO $ readIORef currentDelegateStateRef
          case delegateState of
            Initialized headId _ -> do
              dynamic <-
                liftIO $
                  constructTermsDynamic
                    actor
                    txIn
                    (headIdToCurrencySymbol headId)
              liftIO $ writeAuctionTermsDynamic auctionName dynamic
              config <-
                liftIO $
                  noteM ("could not read auction terms config for " <> show auctionName) $
                    readAuctionTermsConfig auctionName
              terms <- liftIO $ configToAuctionTerms config dynamic
              liftIO . putStrLn $
                show actor
                  <> " announces auction called "
                  <> show auctionName
                  <> "."
              announceAuction terms
            _ -> liftIO . putStrLn $ "Hydra is not initialized yet"
        Nothing -> liftIO . putStrLn $ "User doesn't have the \"Mona Lisa\" token.\nThis demo is configured to use this token as the auction lot."
    StartBidding auctionName -> do
      terms <- auctionTermsFor auctionName
      doOnMatchingStage terms BiddingStartedStage $ do
        liftIO . putStrLn $
          show actor
            <> " starts the bidding phase of auction "
            <> show auctionName
            <> "."
        startBidding terms
    MoveToL2 auctionName -> do
      terms <- auctionTermsFor auctionName
      doOnMatchingStage terms BiddingStartedStage $ do
        mUtxo <- scriptSingleUtxo StandingBid terms
        (standingBidTxIn, _) <- case mUtxo of
          Just x -> return x
          Nothing -> fail "No Standing bid found"
        liftIO $
          sendRequestToDelegate $
            DelegateInterface.CommitStandingBid
              { auctionTerms = terms
              , utxoToCommit = standingBidTxIn
              }
    NewBid auctionName bidAmount layer -> do
      CliEnhancedAuctionTerms {terms, sellerActor} <-
        liftIO $
          noteM ("could not read enhanced auction terms for " <> show auctionName) $
            readCliEnhancedAuctionTerms auctionName
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
          sellerSignature <- liftIO $ sellerSignatureForActor terms actor
          doOnMatchingStage terms BiddingStartedStage $
            case layer of
              L1 -> newBid terms bidAmount sellerSignature
              L2 -> do
                (_, _, bidderSigningKey) <- addressAndKeys
                let bidDatum =
                      createStandingBidDatum terms bidAmount sellerSignature bidderSigningKey
                liftIO $
                  sendRequestToDelegate $
                    DelegateInterface.NewBid
                      { auctionTerms = terms
                      , datum = bidDatum
                      }
    MakeDeposit auctionName depositAmount -> do
      terms <- auctionTermsFor auctionName
      doOnMatchingStage terms AnnouncedStage $
        mkDeposit terms depositAmount
    BidderBuys auctionName -> do
      terms <- auctionTermsFor auctionName
      liftIO . putStrLn $
        show actor
          <> " buys the auction lot, as the winning bidder."
      doOnMatchingStage terms BiddingEndedStage $ do
        mWinningBidderPk <- currentWinningBidder terms
        (currentActorAddress, _, _) <- addressAndKeys
        case mWinningBidderPk of
          Just winningBidderPk -> do
            winningBidderAddress <-
              fromPlutusAddressInMonad $
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
    BidderClaimsDeposit auctionName -> do
      terms <- auctionTermsFor auctionName
      liftIO . putStrLn $
        show actor
          <> " reclaims their deposit, as a losing bidder."
      doOnMatchingStage terms BiddingEndedStage $
        losingBidderClaimDeposit terms
    SellerReclaims auctionName -> do
      liftIO . putStrLn $
        show actor
          <> " reclaims the auction lot, as the seller."
      terms <- auctionTermsFor auctionName
      doOnMatchingStage terms VoucherExpiredStage $
        sellerReclaims terms
      liftIO . putStrLn $
        show actor
          <> " now has the following utxos in their wallet."
      utxos <- actorTipUtxo
      liftIO $ prettyPrintUtxo utxos
    SellerClaimsDepositFor auctionName bidderActor -> do
      terms <- auctionTermsFor auctionName
      liftIO . putStrLn $
        show actor
          <> ", as the seller, reclaims the deposit for "
          <> show bidderActor
      bidderPKH <- liftIO $ getActorPubKeyHash bidderActor
      doOnMatchingStage terms VoucherExpiredStage $
        sellerClaimDepositFor terms bidderPKH
    Cleanup auctionName -> do
      terms <- auctionTermsFor auctionName
      liftIO . putStrLn $
        "Cleaning up all remaining script utxos for auction "
          <> show auctionName
          <> "."
      doOnMatchingStage terms CleanupStage $
        cleanupTx terms

noteM :: forall m a. MonadFail m => String -> m (Maybe a) -> m a
noteM s = (>>= maybe (fail s) pure)

auctionTermsFor :: forall m. MonadIO m => AuctionName -> m AuctionTerms
auctionTermsFor name =
  liftIO $
    noteM ("could not read auction terms for " <> show name) $
      readAuctionTerms name
