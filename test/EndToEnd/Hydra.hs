module EndToEnd.Hydra (testSuite) where

-- Prelude imports

import Hydra.Prelude (
  Contravariant (contramap),
  Enum (toEnum),
  Eq ((==)),
  Foldable (toList),
  Functor (fmap),
  Int,
  Maybe (Just),
  MonadIO (liftIO),
  MonadReader (ask),
  MonadTime (getCurrentTime),
  Monoid (mempty),
  Num ((*), (+), (-)),
  RealFrac (truncate),
  Semigroup ((<>)),
  ToJSON (toJSON),
  diffUTCTime,
  either,
  guard,
  id,
  show,
  ($),
  (.),
  (<$>),
 )
import Test.Hydra.Prelude (failAfter, failure, withTempDir)
import Prelude (putStrLn, return)
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
import Data.Map qualified as Map
import Data.Set qualified as Set

-- Haskell test imports
import Test.QuickCheck (generate)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- Hydra imports
import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (waitForUTxO)
import CardanoNode (
  RunningNode (
    networkId,
    nodeSocket
  ),
 )
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
import Hydra.ContestationPeriod (
  ContestationPeriod (UnsafeContestationPeriod),
 )
import Hydra.Crypto (generateSigningKey)
import Hydra.Ledger (txId)
import Hydra.Ledger.Cardano (genKeyPair, mkSimpleTx)
import Hydra.Party (deriveParty)
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
import HydraAuction.Runner (
  EndToEndLog (..),
  ExecutionContext (MkExecutionContext, node, tracer),
  HydraAuctionLog (..),
  Runner,
 )

-- Hydra auction test imports
import EndToEnd.Utils (mkAssertion)

testSuite :: TestTree
testSuite =
  testGroup
    "L2"
    [testCase "basic-hydra-tx" basicHydraTxTest]

basicHydraTxTest :: Assertion
basicHydraTxTest = mkAssertion $ do
  MkExecutionContext {node} <- ask
  hydraScriptsTxId <- liftIO $ publishHydraScriptsAs node Faucet
  initAndClose 0 hydraScriptsTxId

initAndClose :: Int -> TxId -> Runner ()
initAndClose clusterIx hydraScriptsTxId = do
  MkExecutionContext {node, tracer} <- ask
  let hydraTracer = contramap FromHydra tracer
  liftIO $
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

      withHydraCluster
        hydraTracer
        tmpDir
        (nodeSocket node)
        firstNodeId
        cardanoKeys
        hydraKeys
        hydraScriptsTxId
        contestationPeriod
        $ \nodes -> do
          putStrLn "START"
          [n1, n2, n3] <- Prelude.return $ toList nodes
          waitForNodesConnected hydraTracer [n1, n2, n3]
          putStrLn "START0"

          -- Funds to be used as fuel by Hydra protocol transactions
          let faucetTracer = contramap FromFaucet hydraTracer
          seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel faucetTracer
          seedFromFaucet_ node bobCardanoVk 100_000_000 Fuel faucetTracer
          seedFromFaucet_ node carolCardanoVk 100_000_000 Fuel faucetTracer

          send n1 $ input "Init" []
          putStrLn "START2"

          waitFor hydraTracer 30 [n1, n2, n3] $
            output
              "HeadIsInitializing"
              ["parties" .= Set.fromList [alice, bob, carol]]

          -- Get some UTXOs to commit to a head
          committedUTxOByAlice <-
            seedFromFaucet node aliceCardanoVk 20_000_000 Normal faucetTracer

          committedUTxOByBob <-
            seedFromFaucet node bobCardanoVk 5_000_000 Normal faucetTracer

          send n1 $ input "Commit" ["utxo" .= committedUTxOByAlice]
          send n2 $ input "Commit" ["utxo" .= Object mempty]
          send n3 $ input "Commit" ["utxo" .= Object mempty]
          waitFor hydraTracer 10 [n1, n2, n3] $
            output
              "HeadIsOpen"
              ["utxo" .= (committedUTxOByAlice)]

          -- NOTE(AB): this is partial and will fail if we are not able to
          -- generate a payment
          let firstCommittedUTxO =
                Prelude.head $ UTxO.pairs committedUTxOByAlice
          let tx =
                either (Prelude.error "Impossible happened") id $
                  mkSimpleTx
                    firstCommittedUTxO
                    (inHeadAddress bobCardanoVk, lovelaceToValue 1_000_000)
                    aliceCardanoSk

          send n1 $ input "NewTx" ["transaction" .= tx]
          waitFor hydraTracer 10 [n1, n2, n3] $
            output "TxSeen" ["transaction" .= tx]

          -- The expected new utxo set is the created payment to bob,
          -- alice's remaining utxo in head and whatever bot has
          -- committed to the head
          let newUTxO =
                Map.fromList
                  [
                    ( TxIn (txId tx) (toEnum 0)
                    , object
                        [ "address"
                            .= String
                              (serialiseAddress $ inHeadAddress bobCardanoVk)
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
                        [ "address"
                            .= String
                              (serialiseAddress $ inHeadAddress aliceCardanoVk)
                        , "value"
                            .= object
                              ["lovelace" .= int (20_000_000 - 1_000_000)]
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
          waitFor hydraTracer 10 [n1] $ output "GetUTxOResponse" ["utxo" .= newUTxO]

          send n1 $ input "Close" []
          deadline <- waitMatch 3 n1 $ \v -> do
            guard $ v ^? key "tag" == Just "HeadIsClosed"
            snapshotNumber <- v ^? key "snapshotNumber"
            guard $ snapshotNumber == toJSON expectedSnapshotNumber
            v ^? key "contestationDeadline" . _JSON

          -- Expect to see ReadyToFanout within 3 seconds after deadline
          remainingTime <- diffUTCTime deadline <$> getCurrentTime
          waitFor hydraTracer (truncate $ remainingTime + 3) [n1] $
            output "ReadyToFanout" []

          send n1 $ input "Fanout" []
          waitFor hydraTracer 3 [n1] $
            output "HeadIsFinalized" ["utxo" .= newUTxO]

          case fromJSON $ toJSON newUTxO of
            Error err ->
              failure $ "newUTxO isn't valid JSON?: " <> err
            Success u ->
              failAfter 5 $ waitForUTxO (networkId node) (nodeSocket node) u
  where
    int :: Int -> Int
    int = id

    inHeadAddress :: VerificationKey PaymentKey -> AddressInEra
    inHeadAddress = mkVkAddress $ Testnet (NetworkMagic 14)
