module HydraAuctionUtils.Hydra.Interface (
  getHydraEventKind,
  HydraEventKind (..),
  HydraEvent,
  HydraCommand,
  HydraProtocol,
  HydraConnectionConfig (..),
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Hydra imports
-- Orphan `IsChainState Tx` needed for `Show ServerOutput` instance imported

import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (Tx)
import Hydra.Chain.Direct.State ()

-- HydraAuction imports
import HydraAuctionUtils.WebSockets.Protocol (Protocol (..))

type HydraCommand = ClientInput Tx
type HydraEvent = ServerOutput Tx

data HydraProtocol

newtype HydraConnectionConfig = MkHydraConnectionConfig
  { retrieveHistory :: Bool
  }

instance Protocol HydraProtocol where
  type Input HydraProtocol = HydraCommand
  type Output HydraProtocol = HydraEvent
  type OutputKind HydraProtocol = HydraEventKind
  type ConnectionConfig HydraProtocol = HydraConnectionConfig
  getOutputKind = getHydraEventKind
  configToConnectionPath _ config =
    "/history=" <> (if retrieveHistory config then "yes" else "no")

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
