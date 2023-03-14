module HydraAuction.Hydra where

-- Prelude imports
import Prelude

-- Haskell imports

import Control.Lens ((^?))
import Control.Monad (guard)
import Control.Tracer (Tracer)
import Data.Aeson (
  Value (Object),
  parseJSON,
  (.=),
 )
import Data.Aeson.Lens (key)
import Data.Aeson.Types (parseMaybe)
import Data.Map qualified as Map
import GHC.Natural (Natural)

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO
import CardanoNode (RunningNode)

-- Hydra imports

import Hydra.Cardano.Api (
  Address,
  BuildTx,
  BuildTxWith,
  PaymentKey,
  ShelleyAddr,
  SigningKey,
  Tx,
  TxId,
  WitCtxTxIn,
  Witness,
 )
import Hydra.Chain.Direct.State (ChainContext)
import Hydra.Party (Party)
import HydraNode (
  EndToEndLog,
  HydraClient,
  input,
  output,
  send,
  waitFor,
  waitMatch,
 )

-- HydraAuction imports

import Control.Concurrent.Async (wait)
import HydraAuction.Hydra.Interface (HydraCommand (..), HydraEvent (..))
import HydraAuction.HydraExtras (submitAndAwaitCommitTx)

fixmeInitAndCommitUsingAllNodes ::
  RunningNode ->
  Party ->
  [HydraClient] ->
  (UTxO.UTxO, SigningKey PaymentKey) ->
  (UTxO.UTxO, BuildTxWith BuildTx (Witness WitCtxTxIn)) ->
  ChainContext ->
  Address ShelleyAddr ->
  IO ()
fixmeInitAndCommitUsingAllNodes
  node
  party1
  [n1, n2, n3]
  (!moneyUtxo, !moneySk)
  (!scriptUtxo, !scriptWitness)
  chainContext
  changeAddress = do
    HeadIsInitializing headId <- sendCommandAndWait n1 Init

    submitAndAwaitCommitTx
      node
      headId
      chainContext
      party1
      (moneyUtxo, moneySk)
      (scriptUtxo, scriptWitness)
      changeAddress

    sendCommand n2 Commit
    sendCommand n3 Commit

    HeadIsOpen <- waitForNewEvent n1 5

    return ()

sendCommand :: HydraClient -> HydraCommand -> IO ()
sendCommand node command =
  send node $ input commandText commandArguments
  where
    commandText = case command of
      Init -> "Init"
      Commit -> "Commit"
      GetUTxO -> "GetUTxO"
      Close -> "Close"
      Fanout -> "Fanout"
    commandArguments = case command of
      Commit -> ["utxo" .= Object mempty]
      _ -> []

sendCommandAndWait :: HydraClient -> HydraCommand -> IO HydraEvent
sendCommandAndWait node command = do
  sendCommand node command
  waitForNewEvent node 5

waitForNewEvent :: HydraClient -> Natural -> IO HydraEvent
waitForNewEvent node timeout =
  waitMatch timeout node $ \v ->
    case v ^? key "tag" of
      Just "GetUTxOResponse" ->
        GetUTxOResponse
          <$> do
            utxoValue <- v ^? key "utxo"
            txInOutMap <- parseMaybe parseJSON utxoValue
            return $ UTxO.fromPairs $ Map.toList txInOutMap
      Just "HeadIsInitializing" -> do
        headIdValue <- v ^? key "headId"
        headId <- parseMaybe parseJSON headIdValue
        return $ HeadIsInitializing headId
      Just "HeadIsOpen" -> Just HeadIsOpen
      Just "HeadIsClosed" -> Just HeadIsClosed
      Just "ReadyToFanout" -> Just ReadyToFanout
      Just "HeadIsFinalized" -> Just HeadIsFinalized
      _ -> Nothing

-- FIXME: refactor using same primitives

postTx :: Tracer IO EndToEndLog -> HydraClient -> Tx -> IO ()
postTx hydraTracer node tx = do
  send node $ input "NewTx" ["transaction" .= tx]
  waitFor hydraTracer 10 [node] $
    output "TxSeen" ["transaction" .= tx]
