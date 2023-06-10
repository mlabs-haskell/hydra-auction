{-# LANGUAGE StrictData #-}

module HydraAuctionUtils.Hydra.Interface (
  getHydraEventKind,
  HydraEventKind (..),
  HydraEvent,
  HydraCommand,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Hydra imports

import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (Tx)
import Hydra.Chain.Direct.State ()

type HydraCommand = ClientInput Tx
type HydraEvent = ServerOutput Tx

data HydraEventKind
  = GetUTxOResponseKind
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
  | GreetingsKind
  | PeerConnectedKind
  | PeerDisconnectedKind
  | HeadIsContestedKind
  deriving stock (Eq, Show)

getHydraEventKind :: HydraEvent -> HydraEventKind
getHydraEventKind event = case event of
  GetUTxOResponse {} -> GetUTxOResponseKind
  TxValid {} -> TxValidKind
  TxInvalid {} -> TxInvalidKind
  InvalidInput {} -> InvalidInputKind
  PostTxOnChainFailed {} -> PostTxOnChainFailedKind
  CommandFailed {} -> CommandFailedKind
  SnapshotConfirmed {} -> SnapshotConfirmedKind
  Committed {} -> CommittedKind
  HeadIsInitializing {} -> HeadIsInitializingKind
  HeadIsOpen {} -> HeadIsOpenKind
  HeadIsClosed {} -> HeadIsClosedKind
  ReadyToFanout {} -> ReadyToFanoutKind
  HeadIsFinalized {} -> HeadIsFinalizedKind
  HeadIsAborted {} -> HeadIsAbortedKind
  Greetings {} -> GreetingsKind
  PeerConnected {} -> PeerConnectedKind
  PeerDisconnected {} -> PeerDisconnectedKind
  HeadIsContested {} -> HeadIsContestedKind
