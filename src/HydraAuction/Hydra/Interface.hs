{-# LANGUAGE StrictData #-}

module HydraAuction.Hydra.Interface (
  commandConstructorName, getHydraEventKind, HydraEventKind (..), HydraEvent (..), HydraCommand (..)) where

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
  deriving stock (Show)

commandConstructorName :: HydraCommand -> Text
commandConstructorName Init = "Init"
commandConstructorName GetUTxO = "GetUTxO"
commandConstructorName (NewTx _) = "NewTx"
commandConstructorName (Commit _) = "Commit"
commandConstructorName Close = "Close"
commandConstructorName Fanout = "Fanout"

-- FIXME: Add UTxO to HeadIsFinalized
data HydraEvent
  = GetUTxOResponse UTxO.UTxO
  | TxSeen Tx
  | Committed
  | HeadIsInitializing HeadId
  | HeadIsOpen
  | HeadIsClosed
  | NodeCommitted
  | ReadyToFanout
  | HeadIsFinalized

data HydraEventKind
  = GetUTxOResponseKind
  | TxSeenKind
  | CommittedKind
  | HeadIsInitializingKind
  | HeadIsOpenKind
  | HeadIsClosedKind
  | ReadyToFanoutKind
  | HeadIsFinalizedKind
  deriving stock (Eq)


getHydraEventKind :: HydraEvent -> HydraEventKind
getHydraEventKind event = case event of
  GetUTxOResponse _ -> GetUTxOResponseKind
  TxSeen _ -> TxSeenKind
  Committed -> CommittedKind
  HeadIsInitializing _ -> HeadIsInitializingKind
  HeadIsOpen -> HeadIsOpenKind
  HeadIsClosed -> HeadIsClosedKind
  ReadyToFanout -> ReadyToFanoutKind
  HeadIsFinalized -> HeadIsFinalizedKind
