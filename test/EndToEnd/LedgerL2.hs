{-# LANGUAGE PartialTypeSignatures #-}

module EndToEnd.LedgerL2 (testSuite) where

-- Prelude imports
import Hydra.Prelude (ask, contramap, liftIO)
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

import HydraAuction.Hydra.Interface (HydraCommand (..), HydraEvent (..), HydraEventKind (..))
import HydraAuction.Hydra.Monad (
  AwaitedHydraEvent (..),
  sendCommandAndWaitFor,
  waitForHydraEvent,
 )
import HydraAuction.Hydra.Runner (executeRunnerInTest)
import HydraAuction.HydraExtras (submitAndAwaitCommitTx)
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
import HydraAuction.Tx.StandingBid (newBidTx)
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
import HydraAuctionUtils.Fixture qualified as AuctionFixture

-- Hydra auction test imports
import EndToEnd.HydraUtils (
  commitWithCollateralAdaFor,
  runningThreeNodesHydra,
 )
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
    $ \(hydraNodesStuff, chainContext) -> do
      [(p1, n1, _n1Actor), (p2, n2, n2Actor), (p3, n3, n3Actor)] <- return hydraNodesStuff
      -- Create

      utxoRef <- executeRunner ctx $ withActor seller $ do
        -- liftIO $ threadDelay 3

        nftTx <- mintOneTestNFT
        return $ mkTxIn nftTx 0

      terms <- do
        dynamicState <- constructTermsDynamic seller utxoRef
        configToAuctionTerms config dynamicState

      standingBidUtxo <-
        executeRunner ctx $ withActor seller $ do
          announceAuction terms

          waitUntil $ biddingStart terms
          startBidding terms

          scriptUtxos StandingBid terms

      -- Move

      let script =
            fromPlutusScript @PlutusScriptV2 $
              getValidator $
                standingBidValidator terms
          standingBidWitness = mkInlinedDatumScriptWitness script MoveToHydra

      actorUtxo <- executeRunner ctx $ withActor seller $ actorTipUtxo
      let moneyUtxo = UTxO.fromPairs [Prelude.head (UTxO.pairs actorUtxo)]

      HeadIsInitializing headId <-
        executeRunnerInTest n1 $
          sendCommandAndWaitFor Any Init

      _ <-
        executeRunner ctx $
          submitAndAwaitCommitTx
            node
            headId
            chainContext
            p1
            (moneyUtxo, sellerSk)
            (standingBidUtxo, standingBidWitness)
            sellerAddress

      executeRunnerInTest n2 $
        commitWithCollateralAdaFor node n2Actor
      executeRunnerInTest n3 $
        commitWithCollateralAdaFor node n3Actor

      HeadIsOpen <-
        executeRunnerInTest n1 $
          waitForHydraEvent (SpecificKind HeadIsOpenKind)

      -- New bid
      headUtxo <- liftIO $ executeRunnerInTest n1 $ do
        GetUTxOResponse utxo <-
          sendCommandAndWaitFor (SpecificKind GetUTxOResponseKind) GetUTxO
        return utxo
      let standingBid = headUtxo

      -- FIXME: not working due to cardano-api issue
      newBidTx' <- executeRunner ctx $ withActor bidder1 $ do
         newBidTx terms (startingBid terms) standingBid
      liftIO $ executeRunnerInTest n1 $
          submitAndAwaitTx newBidTx'

      -- Close Head
      liftIO $ executeRunnerInTest n1 $ do
        HeadIsClosed <- sendCommandAndWaitFor Any Close
        ReadyToFanout <- waitForHydraEvent Any
        HeadIsFinalized <- sendCommandAndWaitFor Any Fanout
        return ()
