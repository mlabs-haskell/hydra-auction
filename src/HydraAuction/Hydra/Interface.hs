{-# LANGUAGE StrictData #-}

module HydraAuction.Hydra.Interface (HydraEvent (..), HydraCommand (..)) where

-- Prelude imports
import Prelude

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

-- FIXME: Add Commited, and add UTxO to HeadIsFinalized
data HydraEvent
  = GetUTxOResponse UTxO.UTxO
  | TxSeen Tx
  | HeadIsInitializing HeadId
  | HeadIsOpen
  | HeadIsClosed
  | NodeCommitted
  | ReadyToFanout
  | HeadIsFinalized
