{-# LANGUAGE StrictData #-}

module HydraAuction.Hydra.Interface (
  commandConstructorName,
  getHydraEventKind,
  HydraEventKind (..),
  HydraEvent (..),
  HydraCommand (..),
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Data.Text.Internal (Text)

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports

import Hydra.Cardano.Api (Tx)
import Hydra.Chain (HeadId)

data HydraCommand
  = Init
  | GetUTxO
  | NewTx Tx
  | Commit UTxO.UTxO
  | Close
  | Fanout
  | Abort
  deriving stock (Show)

commandConstructorName :: HydraCommand -> Text
commandConstructorName Init = "Init"
commandConstructorName GetUTxO = "GetUTxO"
commandConstructorName (NewTx _) = "NewTx"
commandConstructorName (Commit _) = "Commit"
commandConstructorName Close = "Close"
commandConstructorName Fanout = "Fanout"
commandConstructorName Abort = "Abort"

-- FIXME: Add UTxO to HeadIsFinalized
data HydraEvent
  = GetUTxOResponse UTxO.UTxO
  | TxSeen Tx
  | TxValid Tx
  | TxInvalid Tx
  | InvlidInput {reason :: String, invalidInput :: String}
  | PostTxOnChainFailed {txTag :: String, errorTag :: String}
  | CommandFailed {clientInputTag :: String}
  | SnapshotConfirmed
      { txs :: [Tx]
      , utxo :: UTxO.UTxO
      }
  | Committed UTxO.UTxO
  | HeadIsInitializing HeadId
  | HeadIsOpen UTxO.UTxO
  | HeadIsClosed
  | ReadyToFanout
  | HeadIsFinalized UTxO.UTxO
  | HeadIsAborted
  deriving stock (Eq, Show)

data HydraEventKind
  = GetUTxOResponseKind
  | TxSeenKind
  | TxValidKind
  | TxInvalidKind
  | InvlidInputKind
  | PostTxOnChainFailedKind
  | CommandFailedKind
  | SnapshotConfirmedKind
  | CommittedKind
  | HeadIsInitializingKind
  | HeadIsOpenKind
  | HeadIsClosedKind
  | ReadyToFanoutKind
  | HeadIsFinalizedKind
  | HeadIsAbortedKind
  deriving stock (Eq, Show)

getHydraEventKind :: HydraEvent -> HydraEventKind
getHydraEventKind event = case event of
  GetUTxOResponse _ -> GetUTxOResponseKind
  TxSeen _ -> TxSeenKind
  TxValid _ -> TxValidKind
  TxInvalid _ -> TxInvalidKind
  InvlidInput {} -> InvlidInputKind
  PostTxOnChainFailed {} -> PostTxOnChainFailedKind
  CommandFailed {} -> CommandFailedKind
  SnapshotConfirmed {} -> SnapshotConfirmedKind
  Committed _ -> CommittedKind
  HeadIsInitializing _ -> HeadIsInitializingKind
  HeadIsOpen {} -> HeadIsOpenKind
  HeadIsClosed -> HeadIsClosedKind
  ReadyToFanout -> ReadyToFanoutKind
  HeadIsFinalized {} -> HeadIsFinalizedKind
  HeadIsAborted -> HeadIsAbortedKind
