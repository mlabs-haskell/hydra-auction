{-# LANGUAGE PartialTypeSignatures #-}

module EndToEnd.LedgerL2 (testSuite) where

-- Prelude imports
import Hydra.Prelude -- TODO
import Test.Hydra.Prelude (failAfter, failure, withTempDir)
import Prelude qualified

-- Haskell imports

import Control.Lens ((^?))
import Data.Aeson (
  Result (..),
  Value (Null, Object, String),
  fromJSON,
  object,
  (.=),
 )
import Data.Aeson.Lens (key, _JSON)
import Data.Aeson.Types (parseMaybe)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set

-- Haskell test imports
import Test.QuickCheck (generate)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- Plutus imports
import Plutus.V2.Ledger.Api (getValidator)

-- Hydra imports
import Cardano.Api.UTxO qualified as UTxO
import CardanoClient -- TODO
import CardanoClient (waitForUTxO)
import CardanoNode (
  RunningNode (
    RunningNode,
    networkId,
    nodeSocket
  ),
 )
import Hydra.Cardano.Api (
  AddressInEra,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  PlutusScript (..),
  PlutusScriptV2,
  TxId,
  TxIn (..),
  VerificationKey,
  fromPlutusScript,
  lovelaceToValue,
  makeShelleyKeyWitness,
  makeSignedTransaction,
  mkTxIn,
  mkVkAddress,
  serialiseAddress,
  toPlutusValue,
  txOutValue,
  pattern PlutusScript,
  pattern Tx,
  pattern TxBody,
  pattern WitnessPaymentKey,
 )
import Hydra.Chain.Direct (loadChainContext)
import Hydra.Chain.Direct.State (ChainContext (..))
import Hydra.Chain.Direct.Tx (mkHeadId)
import Hydra.Cluster.Faucet (
  Marked (Fuel, Normal),
  publishHydraScriptsAs,
  seedFromFaucet,
  seedFromFaucet_,
 )
import Hydra.Cluster.Util (chainConfigFor)
import Hydra.ContestationPeriod (
  ContestationPeriod (UnsafeContestationPeriod),
 )
import Hydra.Contract.Head qualified as Head
import Hydra.Crypto (generateSigningKey, verificationKeyHash)
import Hydra.Ledger (txId)
import Hydra.Ledger.Cardano (genKeyPair, mkSimpleTx)
import Hydra.Party (Party, deriveParty)
import HydraAuction.OnChain (AuctionScript (StandingBid), standingBidValidator)
import HydraAuction.Tx.Common (mkInlinedDatumScriptWitness, scriptUtxos, submitAndAwaitTx)
import HydraAuction.Tx.TestNFT (mintOneTestNFT)
import HydraNode (
  input,
  output,
  send,
  waitFor,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
 )

-- Hydra auction imports

import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (keysFor)
import HydraAuction.Fixture qualified as AuctionFixture
import HydraAuction.HydraExtras (submitAndAwaitCommitTx)
import HydraAuction.OnChain.TestNFT (testNftAssetClass)
import HydraAuction.Runner (
  EndToEndLog (..),
  ExecutionContext (MkExecutionContext, node, tracer),
  HydraAuctionLog (..),
  Runner,
  executeRunner,
  initWallet,
  withActor,
 )
import HydraAuction.Runner.Time (waitUntil)
import HydraAuction.Tx.Common (callBodyAutoBalance)
import HydraAuction.Tx.Escrow (
  announceAuction,
  bidderBuys,
  sellerReclaims,
  startBidding,
 )
import HydraAuction.Tx.StandingBid (cleanupTx, newBid)
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
import HydraAuction.Types (
  AuctionTerms (..),
  StandingBidRedeemer (MoveToHydra),
  intToNatural,
 )

-- Hydra auction test imports
import EndToEnd.Utils (mkAssertion)

testSuite :: TestTree
testSuite =
  testGroup
    "Ledger-L2"
    [testCase "bidder-buys" bidderBuysTest]

bidderBuysTest :: Assertion
bidderBuysTest = mkAssertion $ do
  -- TODO: refactor to utils
  MkExecutionContext {node} <- ask
  hydraScriptsTxId <- liftIO $ publishHydraScriptsAs node Faucet
  bidderBuysTest' 0 hydraScriptsTxId

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

bidderBuysTest' :: Int -> TxId -> Runner ()
bidderBuysTest' clusterIx hydraScriptsTxId = do
  let actors@[seller, bidder1, bidder2] = [AuctionFixture.Alice, AuctionFixture.Bob, AuctionFixture.Carol]
  [sellerU, bidder1U, bidder2U] <- mapM (initWallet 20_000_000_000) actors

  (_, sellerSk) <- liftIO $ AuctionFixture.keysFor seller

  ctx@MkExecutionContext {node, tracer} <- ask
  let hydraTracer = contramap FromHydra tracer

  runningThreeNodesHydra node clusterIx hydraTracer hydraScriptsTxId $ \(parties, nodes, scriptRegistry) -> do
    executeRunner ctx $ do
      [n1, n2, n3] <- return nodes

      -- Create

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

      liftIO $
        fixmeInitAndCommitUsingAllNodes
          node
          parties
          hydraTracer
          nodes
          (sellerU, sellerSk)
          (standingBidUtxo, standingBidWitness)
          scriptRegistry

      -- Get UTxO
      let standingBid = undefined
      let newBidTx = newBidL2Tx terms 8_000_000
      postTx newBidTx n1

headIdIs v = do
  guard $ v ^? key "tag" == Just "HeadIsInitializing"
  headId <- v ^? key "headId"
  parseMaybe parseJSON headId

-- fixmeInitAndCommitUsingAllNodes :: _ -> [Party] -> _ -> _ -> _ -> _ -> IO _
fixmeInitAndCommitUsingAllNodes
  (node@RunningNode {networkId, nodeSocket})
  parties@[p1, p2, p3]
  hydraTracer
  [n1, n2, n3]
  (!commitedUtxo, !commiterSk)
  (!scriptUtxo, !scriptWitness)
  chainContext = do
    -- TODO: why does not work if placed here?
    -- send n1 $ input "Init" []

    Just !headId <- waitMatch 20 n1 headIdIs

    submitAndAwaitCommitTx node headId chainContext p1 (commitedUtxo, commiterSk) (scriptUtxo, scriptWitness)

    putStrLn "POSTSUBMIT"
    -- FIXME
    send n2 $ input "Commit" ["utxo" .= Object mempty]
    send n3 $ input "Commit" ["utxo" .= Object mempty]

    waitFor hydraTracer 20 [n1, n2, n3] $
      output
        "HeadIsOpen"
        ["utxo" .= (commitedUtxo)]

-- FIXME: do not check parties, cuz hydra-node already check that
-- Wait for ReadyToCommit

postTx hydraTracer node tx = do
  send node $ input "NewTx" ["transaction" .= tx]
  waitFor hydraTracer 10 [node] $
    output "TxSeen" ["transaction" .= tx]

runningThreeNodesHydra cardanoNode clusterIx hydraTracer hydraScriptsTxId cont =
  liftIO $
    withTempDir "end-to-end-init-and-close" $ \tmpDir -> do
      let actors@[aliceA, bobA, carolA] = [Alice, Bob, Carol]

      aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- keysFor aliceA
      bobKeys@(bobCardanoVk, _) <- keysFor bobA
      carolKeys@(carolCardanoVk, _) <- keysFor carolA

      let hydraKeys =
            map
              (\name -> generateSigningKey (name <> show clusterIx))
              ["alice-", "bob-", "carol-"]
      let parties@[p1, p2, p3] = map deriveParty hydraKeys

      let cardanoKeys = [aliceKeys, bobKeys, carolKeys]

      let firstNodeId = clusterIx * 3
      let contestationPeriod = UnsafeContestationPeriod 2

      -- TODO
      chainConfig <- chainConfigFor Alice tmpDir (nodeSocket cardanoNode) [Bob, Carol] contestationPeriod
      chainContext <- loadChainContext chainConfig p1 [p2, p3] hydraScriptsTxId

      withHydraCluster
        hydraTracer
        tmpDir
        (nodeSocket cardanoNode)
        firstNodeId
        cardanoKeys
        hydraKeys
        hydraScriptsTxId
        contestationPeriod
        $ \nodes -> do
          [n1, n2, n3] <- Prelude.return $ toList nodes
          waitForNodesConnected hydraTracer [n1, n2, n3]

          -- Funds to be used as fuel by Hydra protocol transactions
          let faucetTracer = contramap FromFaucet hydraTracer
          seedFromFaucet_ cardanoNode aliceCardanoVk 100_000_000 Fuel faucetTracer
          seedFromFaucet_ cardanoNode bobCardanoVk 100_000_000 Fuel faucetTracer
          seedFromFaucet_ cardanoNode carolCardanoVk 100_000_000 Fuel faucetTracer

          send n1 $ input "Init" []

          -- TODO remove
          seedFromFaucet_ cardanoNode aliceCardanoVk 100_000_000 Normal faucetTracer
          seedFromFaucet_ cardanoNode bobCardanoVk 100_000_000 Normal faucetTracer
          seedFromFaucet_ cardanoNode carolCardanoVk 100_000_000 Normal faucetTracer

          cont (parties, [n1, n2, n3], chainContext)
