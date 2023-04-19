module EndToEnd.Ledger.L2 (testSuite) where

-- Prelude imports
import Hydra.Prelude (ask, liftIO)
import Prelude

-- Haskell imports
import Data.Maybe (fromJust)

-- Haskell test imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- Plutus imports
import Plutus.V2.Ledger.Api (getValidator)

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports
import Hydra.Cardano.Api (
  PlutusScriptV2,
  fromPlutusScript,
  mkTxIn,
 )
import Hydra.Chain.Direct.Tx (headIdToCurrencySymbol)

-- Hydra auction imports
import HydraAuction.Hydra.Interface (HydraCommand (..), HydraEvent (..), HydraEventKind (..))
import HydraAuction.Hydra.Monad (
  AwaitedHydraEvent (..),
  sendCommandAndWaitFor,
  waitForHydraEvent,
 )
import HydraAuction.Hydra.Runner (executeHydraRunnerFakingParams)
import HydraAuction.HydraHacks (prepareScriptRegistry, submitAndAwaitCommitTx)
import HydraAuction.OnChain (AuctionScript (StandingBid), standingBidValidator)
import HydraAuction.Runner (
  ExecutionContext (MkExecutionContext, node),
  executeRunnerWithLocalNode,
  initWallet,
  withActor,
 )
import HydraAuction.Runner.Time (waitUntil)
import HydraAuction.Tx.Common (
  actorTipUtxo,
  mkInlinedDatumScriptWitness,
  scriptUtxos,
 )
import HydraAuction.Tx.Escrow (
  announceAuction,
  startBidding,
 )
import HydraAuction.Tx.StandingBid (newBid')
import HydraAuction.Tx.TermsConfig (
  AuctionTermsConfig (
    AuctionTermsConfig,
    configAuctionFeePerDelegate,
    configDiffBiddingEnd,
    configDiffBiddingStart,
    configDiffCleanup,
    configDiffVoucherExpiry,
    configMinimumBidIncrement,
    configStartingBid
  ),
  configToAuctionTerms,
  constructTermsDynamic,
 )
import HydraAuction.Tx.TestNFT (mintOneTestNFT)
import HydraAuction.Types (
  ApprovedBidders (..),
  AuctionTerms (..),
  StandingBidRedeemer (MoveToHydra),
  intToNatural,
 )
import HydraAuctionUtils.Fixture (Actor (..), getActorsPubKeyHash)
import HydraAuctionUtils.Tx.Utxo (filterNonFuelUtxo)

-- Hydra auction test imports

import EndToEnd.HydraUtils (
  runningThreeNodesDockerComposeHydra,
 )

testSuite :: TestTree
testSuite =
  testGroup
    "Ledger-L2"
    [testCase "bidder-buys" bidderBuysTest]

config :: AuctionTermsConfig
config =
  AuctionTermsConfig
    { configDiffBiddingStart = 2
    , configDiffBiddingEnd = 5
    , configDiffVoucherExpiry = 8
    , configDiffCleanup = 10
    , configAuctionFeePerDelegate = fromJust $ intToNatural 4_000_000
    , configStartingBid = fromJust $ intToNatural 8_000_000
    , configMinimumBidIncrement = fromJust $ intToNatural 8_000_000
    }

bidderBuysTest :: Assertion
bidderBuysTest = do
  -- FIXME: xfail test to work on CI
  liftIO $ runningThreeNodesDockerComposeHydra $ \stuff -> executeRunnerWithLocalNode $ do
    let ( (n1, mainCommiter)
          , (n2, commiter2)
          , (n3, commiter3)
          ) = stuff
    MkExecutionContext {node} <- ask

    -- Prepare Hydra connections

    -- FIXME: should use already deployed registry as real code would
    scriptRegistry <- liftIO $ prepareScriptRegistry node
    liftIO $ putStrLn "Script registry gotten"

    -- Prepare actors

    actors@[seller, _bidder1, _bidder2] <- return [Alice, Bob, Carol]
    mapM_ (initWallet 200_000_000) actors
    liftIO $ putStrLn "Actors initialized"

    -- Init hydra

    HeadIsInitializing headId <-
      executeHydraRunnerFakingParams n1 $
        sendCommandAndWaitFor Any Init

    -- Create

    utxoRef <- withActor seller $ do
      nftTx <- mintOneTestNFT
      return $ mkTxIn nftTx 0

    terms <- liftIO $ do
      dynamicState <-
        constructTermsDynamic seller utxoRef (headIdToCurrencySymbol headId)
      configToAuctionTerms config dynamicState

    -- FIXME: currenly delegates are checked as bidders onchain
    actorsPkh <- liftIO $ getActorsPubKeyHash [commiter2, commiter3]
    [(standingBidTxIn, standingBidTxOut)] <-
      withActor seller $ do
        announceAuction terms

        waitUntil $ biddingStart terms
        startBidding terms (ApprovedBidders actorsPkh)

        UTxO.pairs <$> scriptUtxos StandingBid terms

    -- Move

    -- FIXME: separate to hack function
    let script =
          fromPlutusScript @PlutusScriptV2 $
            getValidator $
              standingBidValidator terms
        standingBidWitness = mkInlinedDatumScriptWitness script MoveToHydra

    _ <-
      withActor mainCommiter $
        submitAndAwaitCommitTx
          scriptRegistry
          headId
          (standingBidTxIn, standingBidTxOut, standingBidWitness)

    commit2 <- withActor commiter2 $ filterNonFuelUtxo <$> actorTipUtxo
    _ <-
      executeHydraRunnerFakingParams n2 $
        sendCommandAndWaitFor
          (SpecificEvent $ Committed commit2)
          (Commit commit2)

    commit3 <- withActor commiter3 $ filterNonFuelUtxo <$> actorTipUtxo
    _ <-
      executeHydraRunnerFakingParams n3 $
        sendCommandAndWaitFor
          (SpecificEvent $ Committed commit3)
          (Commit commit3)

    HeadIsOpen <-
      executeHydraRunnerFakingParams n1 $
        waitForHydraEvent (SpecificKind HeadIsOpenKind)

    -- Signing bid by delegate number 2 using its collateral
    _ <-
      executeHydraRunnerFakingParams n2 $
        newBid' terms commiter2 (startingBid terms)

    -- Close Head
    executeHydraRunnerFakingParams n1 $ do
      HeadIsClosed <-
        sendCommandAndWaitFor (SpecificKind HeadIsClosedKind) Close
      ReadyToFanout <- waitForHydraEvent Any
      HeadIsFinalized <- sendCommandAndWaitFor Any Fanout
      return ()
