module CLI.Actions (
  CliAction (..),
  Layer (..),
  handleCliAction,
  handlePerAuctionAction,
  seedAmount,
  auctionTermsFor,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports

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
  L1Runner,
  initWallet,
 )
import HydraAuctionUtils.Monads (
  MonadCardanoClient,
  fromPlutusAddressInMonad,
 )
import HydraAuctionUtils.Monads.Actors (
  MonadHasActor (..),
  WithActorT,
  actorTipUtxo,
  addressAndKeys,
  withActor,
 )
import HydraAuctionUtils.PrettyPrinting (prettyPrintUtxo)
import HydraAuctionUtils.Tx.Common (transferAda)

-- Hydra auction CLI imports
import CLI.Config (
  AuctionName,
  CliEnhancedAuctionTerms (..),
  readCliEnhancedAuctionTerms,
  writeAuctionTermsDynamic,
 )
import CLI.Printing (
  announceActionExecution,
  prettyPrintCurrentActorUtxos,
 )
import CLI.Types (CliAction (..), Layer (..), PerAuctionCliAction (..))

seedAmount :: Lovelace
seedAmount = 100_000_000

doOnMatchingStageOrLater ::
  (MonadIO m, MonadHasActor m, MonadCardanoClient m) =>
  AuctionTerms ->
  AuctionStage ->
  m () ->
  m ()
doOnMatchingStageOrLater terms requiredStage action = do
  stage <- liftIO $ currentAuctionStage terms
  if stage >= requiredStage
    then action
    else
      liftIO $
        putStrLn
          ( "Wrong stage for this transaction. Now: "
              <> show stage
              <> ", while required: "
              <> show requiredStage
              <> " or later."
          )

doOnMatchingStage ::
  (MonadIO m, MonadHasActor m, MonadCardanoClient m) =>
  AuctionTerms ->
  AuctionStage ->
  m () ->
  m ()
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
  WithActorT L1Runner ()
handleCliAction sendRequestToDelegate currentDelegateStateRef userAction = do
  actor <- askActor
  case userAction of
    Seed -> do
      announceActionExecution userAction
      void $ initWallet seedAmount actor
    Prepare sellerActor -> do
      announceActionExecution userAction
      forM_ allActors $ initWallet seedAmount
      void $ lift $ withActor sellerActor mintOneTestNFT
      prettyPrintCurrentActorUtxos
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
    MintTestNFT -> do
      announceActionExecution userAction
      void mintOneTestNFT
    TransferAda actorTo marked amount ->
      void $ transferAda actorTo marked amount
    PerAuction auctionName action -> do
      delegateState <- liftIO $ readIORef currentDelegateStateRef
      mTxIn <- findTestNFT <$> actorTipUtxo
      canRun <- case delegateState of
        Initialized headId _ ->
          case (action, mTxIn) of
            (AuctionAnounce, Just txIn) -> do
              _ <- createNewAuctionConfig txIn auctionName headId
              return True
            (AuctionAnounce, Nothing) -> do
              liftIO . putStrLn $
                "User doesn't have the \"Mona Lisa\" token.\n"
                  <> "This demo is configured to use this token"
                  <> " as the auction lot."
              return False
            _ -> return True
        _ -> do
          liftIO . putStrLn $ "Hydra is not initialized yet"
          return False
      if canRun
        then do
          terms <- auctionTermsFor' auctionName
          handlePerAuctionAction
            sendRequestToDelegate
            currentDelegateStateRef
            terms
            auctionName
            action
        else liftIO . putStrLn $ "Cannot perform action"
  where
    createNewAuctionConfig txIn auctionName headId = do
      actor <- askActor
      dynamic <-
        liftIO $
          constructTermsDynamic actor txIn (headIdToCurrencySymbol headId)
      liftIO $ writeAuctionTermsDynamic auctionName dynamic

handlePerAuctionAction ::
  (DelegateInterface.FrontendRequest -> IO ()) ->
  IORef DelegateState ->
  CliEnhancedAuctionTerms ->
  AuctionName ->
  PerAuctionCliAction ->
  WithActorT L1Runner ()
handlePerAuctionAction
  sendRequestToDelegate
  _currentDelegateStateRef
  terms'
  auctionName
  action = do
    actor <- askActor
    let CliEnhancedAuctionTerms {sellerActor, terms} = terms'
    let userAction = PerAuction auctionName action
    case action of
      ShowCurrentStage -> do
        liftIO $ do
          stage <- currentAuctionStage terms
          putStrLn $ "Current stage: " <> show stage
      ShowScriptUtxos script -> do
        announceActionExecution userAction
        utxos <- scriptUtxos script terms
        liftIO $ prettyPrintUtxo utxos
      ShowCurrentWinningBidder -> do
        winningActor <- do
          winningBidder <- currentWinningBidder terms
          liftIO $ mapM actorFromPkh winningBidder
        liftIO $ print winningActor
      ShowActorsMinDeposit minDeposit -> do
        allDeposits <- scriptUtxos Deposit terms

        let matchingDatums = (parseBidDepositDatum . snd <$>) . UTxO.pairs $ filterDepositGreaterThan minDeposit allDeposits
        actors <- liftIO $ mapM (actorFromPkh . bidDepositBidder) matchingDatums
        -- FIXME: pretty print on separate lines
        liftIO . putStrLn $
          "Showing actors that satisfy min deposit: "
            <> show actors
      AuctionAnounce -> do
        announceActionExecution userAction
        announceAuction terms
      StartBidding -> do
        doOnMatchingStage terms BiddingStartedStage $ do
          announceActionExecution userAction
          startBidding terms
      MoveToL2 -> do
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
      NewBid bidAmount layer -> do
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
      MakeDeposit depositAmount -> do
        doOnMatchingStage terms AnnouncedStage $
          mkDeposit terms depositAmount
      BidderBuys -> do
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
      BidderClaimsDeposit ->
        doOnMatchingStageOrLater terms BiddingEndedStage $ do
          announceActionExecution userAction
          losingBidderClaimDeposit terms
      CleanupDeposit -> do
        doOnMatchingStage terms CleanupStage $ do
          announceActionExecution userAction
          cleanupDeposit terms
      SellerReclaims -> do
        doOnMatchingStageOrLater terms VoucherExpiredStage $ do
          announceActionExecution userAction
          sellerReclaims terms
          prettyPrintCurrentActorUtxos
      SellerClaimsDepositFor bidderActor -> do
        announceActionExecution userAction
        bidderPKH <- liftIO $ getActorPubKeyHash bidderActor
        doOnMatchingStageOrLater terms VoucherExpiredStage $
          sellerClaimDepositFor terms bidderPKH
      Cleanup -> do
        doOnMatchingStage terms CleanupStage $ do
          announceActionExecution userAction
          cleanupTx
            terms
          prettyPrintCurrentActorUtxos

noteM :: forall m a. MonadFail m => String -> m (Maybe a) -> m a
noteM s = (>>= maybe (fail s) pure)

auctionTermsFor ::
  forall m. MonadIO m => AuctionName -> m AuctionTerms
auctionTermsFor name = terms <$> auctionTermsFor' name

auctionTermsFor' ::
  forall m. MonadIO m => AuctionName -> m CliEnhancedAuctionTerms
auctionTermsFor' name =
  liftIO $
    noteM ("could not read auction terms for " <> show name) $
      readCliEnhancedAuctionTerms name
