{-# LANGUAGE StrictData #-}

module HydraAuctionUtils.Hydra.Interface (
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

import Hydra.API.ClientInput (ClientInput (..))
import Hydra.Cardano.Api (Tx)
import Hydra.Chain (HeadId)
import Hydra.Party (Party)

type HydraCommand = ClientInput Tx

-- FIXME: Add UTxO to HeadIsFinalized
data HydraEvent
  = GetUTxOResponse UTxO.UTxO
  | TxSeen Tx
  | TxValid Tx
  | TxInvalid UTxO.UTxO Tx
  | InvalidInput {reason :: String, invalidInput :: String}
  | PostTxOnChainFailed {txTag :: String, errorTag :: String}
  | CommandFailed {clientInputTag :: String}
  | SnapshotConfirmed
      { txs :: [Tx]
      , utxo :: UTxO.UTxO
      }
  | Committed {utxo :: UTxO.UTxO, party :: Party}
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
  | InvalidInputKind
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
  TxInvalid {} -> TxInvalidKind
  InvalidInput {} -> InvalidInputKind
  PostTxOnChainFailed {} -> PostTxOnChainFailedKind
  CommandFailed {} -> CommandFailedKind
  SnapshotConfirmed {} -> SnapshotConfirmedKind
  Committed {} -> CommittedKind
  HeadIsInitializing _ -> HeadIsInitializingKind
  HeadIsOpen {} -> HeadIsOpenKind
  HeadIsClosed -> HeadIsClosedKind
  ReadyToFanout -> ReadyToFanoutKind
  HeadIsFinalized {} -> HeadIsFinalizedKind
  HeadIsAborted -> HeadIsAbortedKind
