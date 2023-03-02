module CLI.Actions (
  CliInput (..),
  CliAction (..),
  handleCliAction,
  seedAmount,
) where

-- Prelude imports
import Hydra.Prelude (UTCTime, addUTCTime, ask, liftIO)
import Prelude

-- Haskell imports

import Control.Concurrent.AlarmClock (newAlarmClock, setAlarm)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, void)
import Data.Time.Clock.POSIX qualified as POSIXTime
import System.Console.Concurrent (outputConcurrent)
import System.Console.Regions (ConsoleRegion, RegionLayout (..), displayConsoleRegions, openConsoleRegion, setConsoleRegion)

-- Plutus imports
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.V1.Ledger.Time (POSIXTime (..))

-- Hydra imports
import Hydra.Cardano.Api (Lovelace, TxIn, pattern ShelleyAddressInEra)

-- Hydra auction imports
import HydraAuction.Fixture (Actor (..))
import HydraAuction.OnChain (AuctionScript)
import HydraAuction.Runner (
  ExecutionContext (..),
  Runner,
  executeRunner,
  initWallet,
  withActor,
 )
import HydraAuction.Tx.Common
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  currentWinningBidder,
  sellerReclaims,
  startBidding,
 )
import HydraAuction.Tx.StandingBid
import HydraAuction.Tx.TestNFT
import HydraAuction.Types (AuctionStage (..), AuctionTerms (..), Natural)

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
  = ShowCurrentStage !AuctionName
  | ShowScriptUtxos !AuctionName !AuctionScript
  | ShowUtxos
  | ShowAllUtxos
  | ShowCurrentWinnigBidder !AuctionName
  | Seed
  | Prepare !Actor
  | MintTestNFT
  | AuctionAnounce !AuctionName !TxIn
  | Watch !AuctionName
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
  watchLock <- liftIO newEmptyMVar
  case userAction of
    ShowCurrentStage auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      liftIO $ do
        stage <- currentAuctionStage terms
        putStrLn $ "Current stage: " <> show stage
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
    ShowCurrentWinnigBidder auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      -- FIXME: show actor instread of PubKey
      winningBidderPk <- currentWinningBidder terms
      liftIO $ print winningBidderPk
    MintTestNFT ->
      void mintOneTestNFT
    AuctionAnounce auctionName utxo -> do
      dynamic <- liftIO $ constructTermsDynamic actor utxo
      liftIO $ writeAuctionTermsDynamic auctionName dynamic
      -- FIXME: proper error printing
      Just config <- liftIO $ readAuctionTermsConfig auctionName
      terms <- liftIO $ configToAuctionTerms config dynamic
      announceAuction terms
    Watch auctionName -> do
      ctx <- ask
      -- FIXME: proper error printing
      liftIO $
        displayConsoleRegions $ do
          Just terms <- readAuctionTerms auctionName
          stageRegion <- openConsoleRegion Linear
          winnerRegion <- openConsoleRegion Linear
          outputConcurrent $ "Auction: " <> show auctionName <> "\n"
          setTimerUpdateWinningBidder terms winnerRegion ctx
          setTimerUpdateStage terms stageRegion watchLock
          takeMVar watchLock
    StartBidding auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      startBidding terms
    NewBid auctionName bidAmount -> do
      -- FIXME: proper error printing
      Just CliEnhancedAuctionTerms {terms, sellerActor} <- liftIO $ readCliEnhancedAuctionTerms auctionName
      if actor == sellerActor
        then liftIO $ putStrLn "Seller cannot place a bid"
        else
          doOnMatchingStage terms BiddingStartedStage $
            newBid terms bidAmount
    BidderBuys auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
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
    SellerReclaims auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      doOnMatchingStage terms VoucherExpiredStage $
        sellerReclaims terms
      utxos <- actorTipUtxo
      liftIO $ prettyPrintUtxo utxos
    Cleanup auctionName -> do
      -- FIXME: proper error printing
      Just terms <- liftIO $ readAuctionTerms auctionName
      doOnMatchingStage terms VoucherExpiredStage $
        cleanupTx terms

setTimerUpdateWinningBidder :: AuctionTerms -> ConsoleRegion -> ExecutionContext -> IO ()
setTimerUpdateWinningBidder terms region ctx = do
  winningBidderAddress <- executeRunner ctx $ currentWinningBidder terms
  setConsoleRegion region ("Winner: " <> show winningBidderAddress)
  now <- POSIXTime.getCurrentTime
  alarm <- newAlarmClock $ \_ -> setTimerUpdateWinningBidder terms region ctx
  setAlarm alarm (addUTCTime 120 now)

setTimerUpdateStage :: AuctionTerms -> ConsoleRegion -> MVar () -> IO ()
setTimerUpdateStage terms region watchLock = do
  stage <- currentAuctionStage terms
  let nextStage = getNextStage stage
  setConsoleRegion region ("Stage: " <> show stage)
  if nextStage /= stage
    then do
      alarm <- newAlarmClock $ \_ -> setTimerUpdateStage terms region watchLock
      setAlarm alarm (whenIsStageUTC nextStage terms)
    else putMVar watchLock ()
  where
    getNextStage AnnouncedStage = BiddingStartedStage
    getNextStage BiddingStartedStage = BiddingEndedStage
    getNextStage BiddingEndedStage = VoucherExpiredStage
    getNextStage VoucherExpiredStage = VoucherExpiredStage

whenIsStageUTC :: AuctionStage -> AuctionTerms -> UTCTime
whenIsStageUTC BiddingStartedStage terms = posixToUTC $ biddingStart terms
whenIsStageUTC BiddingEndedStage terms = posixToUTC $ biddingEnd terms
whenIsStageUTC VoucherExpiredStage terms = posixToUTC $ voucherExpiry terms
whenIsStageUTC s _ = error $ "whenIsStageUTC called on " <> show s

posixToUTC :: POSIXTime -> UTCTime
posixToUTC = POSIXTime.posixSecondsToUTCTime . fromInteger . (`div` 1000) . getPOSIXTime
