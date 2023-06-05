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
import Data.Text qualified as T

-- Plutus imports
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)

-- Hydra imports

import Hydra.Cardano.Api (
  Lovelace,
  serialiseAddress,
  pattern ShelleyAddressInEra,
 )
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
  cleanupDeposit,
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
import HydraAuctionUtils.Fixture (
  actorFromPkh,
  allActors,
  getActorPubKeyHash,
 )
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
import HydraAuctionUtils.Tx.Common (transferAda)

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
import CLI.Printing (
  announceActionExecution,
  prettyPrintCurrentActorUtxos,
 )
import CLI.Types (CliAction (..), Layer (..))

seedAmount :: Lovelace
seedAmount = 10_000_000_000

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
      announceActionExecution userAction
      void $ initWallet seedAmount actor
    Prepare sellerActor -> do
      announceActionExecution userAction
      forM_ allActors $ initWallet seedAmount
      void $ withActor sellerActor mintOneTestNFT
      prettyPrintCurrentActorUtxos
    ShowScriptUtxos auctionName script -> do
      announceActionExecution userAction
      terms <- auctionTermsFor auctionName
      utxos <- scriptUtxos script terms
      liftIO $ prettyPrintUtxo utxos
    ShowAddress -> do
      (address, _, _) <- addressAndKeys
      liftIO $
        putStrLn $
          "Address for current actor is: "
            <> T.unpack (serialiseAddress address)
    ShowUtxos -> prettyPrintCurrentActorUtxos
    ShowAllUtxos -> do
      announceActionExecution userAction
      forM_ allActors $ \a -> do
        utxos <- withActor a actorTipUtxo
        liftIO $ print a
        liftIO $ prettyPrintUtxo utxos
        liftIO $ putStrLn "\n"
    ShowCurrentWinningBidder auctionName -> do
      terms <- auctionTermsFor auctionName
      winningActor <- do
        winningBidder <- currentWinningBidder terms
        liftIO $ mapM actorFromPkh winningBidder
      liftIO $ print winningActor
    ShowActorsMinDeposit auctionName minDeposit -> do
      terms <- auctionTermsFor auctionName
      allDeposits <- scriptUtxos Deposit terms

      let matchingDatums = (parseBidDepositDatum . snd <$>) . UTxO.pairs $ filterDepositGreaterThan minDeposit allDeposits
      actors <- liftIO $ mapM (actorFromPkh . bidDepositBidder) matchingDatums
      -- FIXME: pretty print on separate lines
      liftIO . putStrLn $
        "Showing actors that satisfy min deposit: "
          <> show actors
    MintTestNFT -> do
      announceActionExecution userAction
      void mintOneTestNFT
    TransferAda actorTo marked amount ->
      void $ transferAda actorTo marked amount
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
              announceActionExecution userAction
              announceAuction terms
            _ -> liftIO . putStrLn $ "Hydra is not initialized yet"
        Nothing -> liftIO . putStrLn $ "User doesn't have the \"Mona Lisa\" token.\nThis demo is configured to use this token as the auction lot."
    StartBidding auctionName -> do
      terms <- auctionTermsFor auctionName
      doOnMatchingStage terms BiddingStartedStage $ do
        announceActionExecution userAction
        startBidding terms
    MoveToL2 auctionName -> do
      terms <- auctionTermsFor auctionName
      doOnMatchingStage terms BiddingStartedStage $ do
        mUtxo <- scriptSingleUtxo StandingBid terms
        stadingBidUtxo <- case mUtxo of
          Just x -> return x
          Nothing -> fail "No Standing bid found"
        liftIO $
          sendRequestToDelegate $
            DelegateInterface.CommitStandingBid
              { auctionTerms = terms
              , utxoToCommit = stadingBidUtxo
              }
    NewBid auctionName bidAmount layer -> do
      CliEnhancedAuctionTerms {terms, sellerActor} <-
        liftIO $
          noteM ("could not read enhanced auction terms for " <> show auctionName) $
            readCliEnhancedAuctionTerms auctionName
      if actor == sellerActor
        then liftIO $ putStrLn "Seller cannot place a bid"
        else do
          announceActionExecution userAction
          -- FIXME: temporaral stub until Platform server
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
                announceActionExecution userAction
                bidderBuys terms
                prettyPrintCurrentActorUtxos
              else
                liftIO $
                  putStrLn "Cannot perform: Other actor is the winning bidder!"
          Nothing ->
            liftIO $ putStrLn "Cannot perform: No bid is placed!"
    BidderClaimsDeposit auctionName -> do
      terms <- auctionTermsFor auctionName
      -- doOnMatchingStage terms BiddingEndedStage $ do
      announceActionExecution userAction
      losingBidderClaimDeposit terms
    CleanupDeposit auctionName -> do
      terms <- auctionTermsFor auctionName
      doOnMatchingStage terms CleanupStage $ do
        announceActionExecution userAction
        cleanupDeposit terms
    SellerReclaims auctionName -> do
      terms <- auctionTermsFor auctionName
      doOnMatchingStage terms VoucherExpiredStage $ do
        announceActionExecution userAction
        sellerReclaims terms
        prettyPrintCurrentActorUtxos
    SellerClaimsDepositFor auctionName bidderActor -> do
      terms <- auctionTermsFor auctionName
      announceActionExecution userAction
      bidderPKH <- liftIO $ getActorPubKeyHash bidderActor
      doOnMatchingStage terms VoucherExpiredStage $
        sellerClaimDepositFor terms bidderPKH
    Cleanup auctionName -> do
      terms <- auctionTermsFor auctionName
      doOnMatchingStage terms CleanupStage $ do
        announceActionExecution userAction
        cleanupTx
          terms
        prettyPrintCurrentActorUtxos

noteM :: forall m a. MonadFail m => String -> m (Maybe a) -> m a
noteM s = (>>= maybe (fail s) pure)

auctionTermsFor :: forall m. MonadIO m => AuctionName -> m AuctionTerms
auctionTermsFor name =
  liftIO $
    noteM ("could not read auction terms for " <> show name) $
      readAuctionTerms name
