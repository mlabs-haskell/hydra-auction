module HydraAuctionUtils.Delegate.Interface (
  DelegateResponse (..),
  HydraHeadInfo (..),
  DelegateEvent (..),
  DelegateState (..),
  DelegateLogicTypes (..),
  InitializedState (..),
  InitializedStateKind (..),
  initializedStateKind,
  RequestIgnoredReason (..),
  ResponseReason (..),
  FrontendRequest (..),
  IncorrectRequestDataReason (..),
  ImposibleEvent (..),
  MissingPrerequisite (..),
  AbortReason (..),
  initialState,
  isFinalState,
  wasOpened,
  wasStopped,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports
import Data.Aeson (FromJSON, ToJSON)

-- Hydra imports

import Hydra.Cardano.Api (Lovelace)
import Hydra.Chain (HeadId)

-- HydraAuction imports
import HydraAuctionUtils.Hydra.Interface (HydraEvent)
import HydraAuctionUtils.Types.Natural (Natural)

type DatatypeInstances x = (Eq x, Show x, Generic x, ToJSON x, FromJSON x)

-- | Common class for encode Client/Hydra-reacting logic of Delegate server
class
  ( DatatypeInstances (CommitAction protocol)
  , DatatypeInstances (TxAction protocol)
  , DatatypeInstances (OpenState protocol)
  , DatatypeInstances (CustomEvent protocol)
  ) =>
  DelegateLogicTypes protocol
  where
  data CommitAction protocol
  data TxAction protocol
  data OpenState protocol

  -- | Server events for custom logic, for example from scheduler
  data CustomEvent protocol

data FrontendRequest protocol
  = QueryCurrentDelegateState
  | SubmitCommit (CommitAction protocol)
  | SubmitTx (TxAction protocol)

deriving stock instance DelegateLogicTypes protocol => Eq (FrontendRequest protocol)
deriving stock instance DelegateLogicTypes protocol => Show (FrontendRequest protocol)
deriving stock instance DelegateLogicTypes protocol => Generic (FrontendRequest protocol)
deriving anyclass instance DelegateLogicTypes protocol => ToJSON (FrontendRequest protocol)
deriving anyclass instance DelegateLogicTypes protocol => FromJSON (FrontendRequest protocol)

data HydraHeadInfo = MkHydraHeadInfo
  { headId :: HeadId
  , delegatesNumber :: Natural
  , auctionFeePerDelegate :: Lovelace
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON)

{- |
   This is an important type.
   It should encode all Delegate server state required for its logic
   Client should have up-to date state, gotten by pushed responses.
-}
data DelegateState protocol
  = NotInitialized
  | -- Hydra calls this Initializing.
    -- This case covers all Head states after it got Init comand
    -- and thus obtained HeadId.
    Initialized HeadId (InitializedState protocol)

deriving stock instance DelegateLogicTypes protocol => Eq (DelegateState protocol)
deriving stock instance DelegateLogicTypes protocol => Show (DelegateState protocol)
deriving stock instance DelegateLogicTypes protocol => Generic (DelegateState protocol)
deriving anyclass instance DelegateLogicTypes protocol => ToJSON (DelegateState protocol)
deriving anyclass instance DelegateLogicTypes protocol => FromJSON (DelegateState protocol)

initialState :: DelegateState protocol
initialState = NotInitialized

isFinalState :: DelegateState protocol -> Bool
isFinalState (Initialized _ state) =
  initializedStateKind state `elem` [FinalizedKind, AbortedKind]
isFinalState NotInitialized = False

wasOpened :: DelegateState protocol -> Bool
wasOpened state = case state of
  NotInitialized -> False
  Initialized _ (AwaitingCommits {}) -> False
  Initialized _ _ -> True

wasStopped :: DelegateState protocol -> Bool
wasStopped state = case state of
  NotInitialized -> False
  Initialized _ initState ->
    initializedStateKind initState
      `elem` [ClosedKind, FinalizedKind, AbortedKind, AbortRequestedKind]

data InitializedState protocol
  = AwaitingCommits {stangingBidWasCommited :: Bool}
  | Open (OpenState protocol)
  | Closed
  | Finalized
  | AbortRequested AbortReason
  | Aborted

deriving stock instance DelegateLogicTypes protocol => Eq (InitializedState protocol)
deriving stock instance DelegateLogicTypes protocol => Show (InitializedState protocol)
deriving stock instance DelegateLogicTypes protocol => Generic (InitializedState protocol)
deriving anyclass instance DelegateLogicTypes protocol => ToJSON (InitializedState protocol)
deriving anyclass instance DelegateLogicTypes protocol => FromJSON (InitializedState protocol)

data InitializedStateKind
  = AwaitingCommitsKind
  | OpenKind
  | ClosedKind
  | FinalizedKind
  | AbortRequestedKind
  | AbortedKind
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

initializedStateKind ::
  forall protocol. InitializedState protocol -> InitializedStateKind
initializedStateKind kind = case kind of
  AwaitingCommits {} -> AwaitingCommitsKind
  Open {} -> OpenKind
  Closed {} -> ClosedKind
  Finalized {} -> FinalizedKind
  AbortRequested {} -> AbortRequestedKind
  Aborted {} -> AbortedKind

data IncorrectRequestDataReason
  = AuctionTermsAreInvalidOrNotMatchingHead
  | InvalidBidTerms
  | TxIdDoesNotExist
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RequestIgnoredReason protocol
  = IncorrectRequestData IncorrectRequestDataReason
  | WrongDelegateState (DelegateState protocol)

deriving stock instance DelegateLogicTypes protocol => Eq (RequestIgnoredReason protocol)
deriving stock instance DelegateLogicTypes protocol => Show (RequestIgnoredReason protocol)
deriving stock instance DelegateLogicTypes protocol => Generic (RequestIgnoredReason protocol)
deriving anyclass instance DelegateLogicTypes protocol => ToJSON (RequestIgnoredReason protocol)
deriving anyclass instance DelegateLogicTypes protocol => FromJSON (RequestIgnoredReason protocol)

-- It is actually possible if one of Delegates is breaking protocol
data ImposibleEvent
  = -- FIXME: add docs and/or split on different cases
    IncorrectCommit
  | IncorrectUtxoOnL2 String
  | IncorrectHydraEvent
  | OnChainInvariantBreaks
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data MissingPrerequisite
  = AdaForCommit
  | HydraInit
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AbortReason
  = ImpossibleHappened ImposibleEvent
  | RequiredHydraRequestFailed
  | PrerequisiteMissing MissingPrerequisite
  | NobodyCommitedInTime
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ResponseReason = WasQueried | Updated
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DelegateResponse protocol
  = CurrentDelegateState ResponseReason (DelegateState protocol)
  | RequestIgnored (RequestIgnoredReason protocol)
  | CustomEventHappened (CustomEvent protocol)

deriving stock instance DelegateLogicTypes protocol => Eq (DelegateResponse protocol)
deriving stock instance DelegateLogicTypes protocol => Show (DelegateResponse protocol)
deriving stock instance DelegateLogicTypes protocol => Generic (DelegateResponse protocol)
deriving anyclass instance DelegateLogicTypes protocol => ToJSON (DelegateResponse protocol)
deriving anyclass instance DelegateLogicTypes protocol => FromJSON (DelegateResponse protocol)

data DelegateEvent protocol
  = Start
  | CustomEvent (CustomEvent protocol)
  | HydraEvent HydraEvent

deriving stock instance DelegateLogicTypes protocol => Eq (DelegateEvent protocol)
deriving stock instance DelegateLogicTypes protocol => Show (DelegateEvent protocol)
