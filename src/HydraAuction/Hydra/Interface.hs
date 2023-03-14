module HydraAuction.Hydra.Interface (HydraEvent (..), HydraCommand (..)) where

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports
import Hydra.Chain (HeadId)

-- FIXME: Commit not only can be empty
data HydraCommand = Init | GetUTxO | Commit | Close | Fanout

-- FIXME: Add Commited, and add UTxO to HeadIsFinalized
data HydraEvent
  = GetUTxOResponse UTxO.UTxO
  | HeadIsInitializing HeadId
  | HeadIsOpen
  | HeadIsClosed
  | ReadyToFanout
  | HeadIsFinalized
