{-# LANGUAGE PartialTypeSignatures #-}

module EndToEnd.LedgerL2 (testSuite) where

-- Prelude imports
import Hydra.Prelude (ask, contramap, liftIO, threadDelay)
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
import CardanoClient (buildAddress)
import CardanoNode (
  RunningNode (networkId),
 )

-- Hydra imports
import Hydra.Cardano.Api (
  PlutusScriptV2,
  fromPlutusScript,
  mkTxIn,
 )

-- Hydra auction imports

import HydraAuction.Fixture qualified as AuctionFixture
import HydraAuction.Hydra (
  fixmeInitAndCommitUsingAllNodes,
  sendCommandAndWait,
  waitForNewEvent,
  postTx,
 )
import HydraAuction.Tx.StandingBid (newBidTx)
import HydraAuction.Hydra.Interface (HydraCommand (..), HydraEvent (..))
import HydraAuction.OnChain (AuctionScript (StandingBid), standingBidValidator)
import HydraAuction.Runner (
  ExecutionContext (MkExecutionContext, node, tracer),
  HydraAuctionLog (..),
  executeRunner,
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
  AuctionTerms (..),
  StandingBidRedeemer (MoveToHydra),
  intToNatural,
 )

-- Hydra auction test imports
import EndToEnd.HydraUtils (runningThreeNodesHydra)
import EndToEnd.Utils (mkAssertion)

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
bidderBuysTest = mkAssertion $ do
  actors@[seller, bidder1, _] <-
    return
      [ AuctionFixture.Dave
      , AuctionFixture.Eve
      , AuctionFixture.Frank
      ]
  _ <- mapM (initWallet 200_000_000_000) actors

  (sellerVk, sellerSk) <- liftIO $ AuctionFixture.keysFor seller

  ctx@MkExecutionContext {node, tracer} <- ask
  let hydraTracer = contramap FromHydra tracer

  let sellerAddress = buildAddress sellerVk (networkId node)

  runningThreeNodesHydra
    node
    hydraTracer
    $ \(parties, hydraNodes, chainContext) -> do
      -- FIXME: more proper solution, like MonadTransControl or something
      executeRunner ctx $
        withActor AuctionFixture.Dave $ do
          [n1, _, _] <- return hydraNodes

          -- Create

          liftIO $ threadDelay 3

          nftTx <- mintOneTestNFT
          let utxoRef = mkTxIn nftTx 0

          terms <- liftIO $ do
            dynamicState <- constructTermsDynamic seller utxoRef
            configToAuctionTerms config dynamicState

          announceAuction terms

          waitUntil $ biddingStart terms
          startBidding terms

          standingBidUtxo <- scriptUtxos StandingBid terms

          -- Move

          let script =
                fromPlutusScript @PlutusScriptV2 $
                  getValidator $ standingBidValidator terms
              standingBidWitness = mkInlinedDatumScriptWitness script MoveToHydra

          actorUtxo <- actorTipUtxo
          let moneyUtxo = UTxO.fromPairs [Prelude.head (UTxO.pairs actorUtxo)]

          p1 : _ <- return parties

          liftIO $
            fixmeInitAndCommitUsingAllNodes
              node
              p1
              hydraNodes
              (moneyUtxo, sellerSk)
              (standingBidUtxo, standingBidWitness)
              chainContext
              sellerAddress

          -- New bid
          standingBid <- liftIO $ do
            GetUTxOResponse utxo <- sendCommandAndWait n1 GetUTxO
            return utxo

          -- FIXME: not working due to cardano-api isse
          withActor bidder1 $ do
            newBidTx <- newBidTx terms (startingBid terms) standingBid
            liftIO $ postTx hydraTracer n1 newBidTx

          -- Close Head
          liftIO $ do
            HeadIsClosed <- sendCommandAndWait n1 Close
            ReadyToFanout <- waitForNewEvent n1 25
            HeadIsFinalized <- sendCommandAndWait n1 Fanout
            return ()
