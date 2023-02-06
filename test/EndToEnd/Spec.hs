{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module EndToEnd.Spec (spec, tests) where

import Hydra.Prelude hiding (threadDelay)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (waitForUTxO)
import CardanoNode (RunningNode (RunningNode, networkId, nodeSocket), withCardanoNodeDevnet)
import Control.Lens ((^?))
import Data.Aeson (Result (..), Value (Null, Object, String), fromJSON, object, (.=))
import Data.Aeson.Lens (key, _JSON)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Hydra.Cardano.Api (
  AddressInEra,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  TxId,
  TxIn (..),
  VerificationKey,
  lovelaceToValue,
  mkVkAddress,
  serialiseAddress,
 )
import Hydra.Cluster.Faucet (
  Marked (Fuel, Normal),
  publishHydraScriptsAs,
  seedFromFaucet,
  seedFromFaucet_,
 )
import Hydra.Cluster.Fixture (
  Actor (Faucet),
 )
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Crypto (generateSigningKey)
import Hydra.Ledger (txId)
import Hydra.Ledger.Cardano (genKeyPair, mkSimpleTx)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Party (deriveParty)
import HydraNode (
  EndToEndLog (..),
  input,
  output,
  send,
  waitFor,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
 )
import Test.QuickCheck (generate)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Prelude qualified

import EndToEnd.Scenario (scenarioSpec)

tests :: IO TestTree
tests = do
  testSpec "Head" spec
  testSpec "Scenario" scenarioSpec

spec :: Spec
spec = around showLogsOnFailure $ do
  describe "End-to-end test using a single cardano-node" $ do
    describe "three hydra nodes scenario" $
      it "inits a Head, processes a single Cardano transaction and closes it again" $ \tracer ->
        failAfter 60 $
          withTempDir "end-to-end-cardano-node" $ \tmpDir -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              hydraScriptsTxId <- publishHydraScriptsAs node Faucet
              initAndClose tracer 0 hydraScriptsTxId node

initAndClose :: Tracer IO EndToEndLog -> Int -> TxId -> RunningNode -> IO ()
initAndClose tracer clusterIx hydraScriptsTxId node@RunningNode {nodeSocket, networkId} = do
  withTempDir "end-to-end-init-and-close" $ \tmpDir -> do
    aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- generate genKeyPair
    bobKeys@(bobCardanoVk, _) <- generate genKeyPair
    carolKeys@(carolCardanoVk, _) <- generate genKeyPair

    let aliceSk = generateSigningKey ("alice-" <> show clusterIx)
    let bobSk = generateSigningKey ("bob-" <> show clusterIx)
    let carolSk = generateSigningKey ("carol-" <> show clusterIx)

    let alice = deriveParty aliceSk
    let bob = deriveParty bobSk
    let carol = deriveParty carolSk

    let cardanoKeys = [aliceKeys, bobKeys, carolKeys]
        hydraKeys = [aliceSk, bobSk, carolSk]

    let firstNodeId = clusterIx * 3
    let contestationPeriod = UnsafeContestationPeriod 2
    withHydraCluster tracer tmpDir nodeSocket firstNodeId cardanoKeys hydraKeys hydraScriptsTxId contestationPeriod $ \nodes -> do
      let [n1, n2, n3] = toList nodes
      waitForNodesConnected tracer [n1, n2, n3]

      -- Funds to be used as fuel by Hydra protocol transactions
      seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
      seedFromFaucet_ node bobCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
      seedFromFaucet_ node carolCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)

      send n1 $ input "Init" []
      waitFor tracer 10 [n1, n2, n3] $
        output "ReadyToCommit" ["parties" .= Set.fromList [alice, bob, carol]]

      -- Get some UTXOs to commit to a head
      committedUTxOByAlice <- seedFromFaucet node aliceCardanoVk 20_000_000 Normal (contramap FromFaucet tracer)
      committedUTxOByBob <- seedFromFaucet node bobCardanoVk 5_000_000 Normal (contramap FromFaucet tracer)
      send n1 $ input "Commit" ["utxo" .= committedUTxOByAlice]
      send n2 $ input "Commit" ["utxo" .= committedUTxOByBob]
      send n3 $ input "Commit" ["utxo" .= Object mempty]
      waitFor tracer 10 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= (committedUTxOByAlice <> committedUTxOByBob)]

      -- NOTE(AB): this is partial and will fail if we are not able to generate a payment
      let firstCommittedUTxO = Prelude.head $ UTxO.pairs committedUTxOByAlice
      let Right tx =
            mkSimpleTx
              firstCommittedUTxO
              (inHeadAddress bobCardanoVk, lovelaceToValue 1_000_000)
              aliceCardanoSk
      send n1 $ input "NewTx" ["transaction" .= tx]
      waitFor tracer 10 [n1, n2, n3] $
        output "TxSeen" ["transaction" .= tx]

      -- The expected new utxo set is the created payment to bob,
      -- alice's remaining utxo in head and whatever bot has
      -- committed to the head
      let newUTxO =
            Map.fromList
              [
                ( TxIn (txId tx) (toEnum 0)
                , object
                    [ "address" .= String (serialiseAddress $ inHeadAddress bobCardanoVk)
                    , "value" .= object ["lovelace" .= int 1_000_000]
                    , "datum" .= Null
                    , "datumhash" .= Null
                    , "inlineDatum" .= Null
                    , "referenceScript" .= Null
                    ]
                )
              ,
                ( TxIn (txId tx) (toEnum 1)
                , object
                    [ "address" .= String (serialiseAddress $ inHeadAddress aliceCardanoVk)
                    , "value" .= object ["lovelace" .= int (20_000_000 - 1_000_000)]
                    , "datum" .= Null
                    , "datumhash" .= Null
                    , "inlineDatum" .= Null
                    , "referenceScript" .= Null
                    ]
                )
              ]
              <> fmap toJSON (Map.fromList (UTxO.pairs committedUTxOByBob))

      let expectedSnapshot =
            object
              [ "snapshotNumber" .= int expectedSnapshotNumber
              , "utxo" .= newUTxO
              , "confirmedTransactions" .= [tx]
              ]
          expectedSnapshotNumber = 1

      waitMatch 10 n1 $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        snapshot <- v ^? key "snapshot"
        guard $ snapshot == expectedSnapshot

      send n1 $ input "GetUTxO" []
      waitFor tracer 10 [n1] $ output "GetUTxOResponse" ["utxo" .= newUTxO]

      send n1 $ input "Close" []
      deadline <- waitMatch 3 n1 $ \v -> do
        guard $ v ^? key "tag" == Just "HeadIsClosed"
        snapshotNumber <- v ^? key "snapshotNumber"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber
        v ^? key "contestationDeadline" . _JSON

      -- Expect to see ReadyToFanout within 3 seconds after deadline
      remainingTime <- diffUTCTime deadline <$> getCurrentTime
      waitFor tracer (truncate $ remainingTime + 3) [n1] $
        output "ReadyToFanout" []

      send n1 $ input "Fanout" []
      waitFor tracer 3 [n1] $
        output "HeadIsFinalized" ["utxo" .= newUTxO]

      case fromJSON $ toJSON newUTxO of
        Error err ->
          failure $ "newUTxO isn't valid JSON?: " <> err
        Success u ->
          failAfter 5 $ waitForUTxO networkId nodeSocket u

--
-- Helpers
--

int :: Int -> Int
int = id

inHeadAddress :: VerificationKey PaymentKey -> AddressInEra
inHeadAddress =
  mkVkAddress network
  where
    network = Testnet (NetworkMagic 14)
