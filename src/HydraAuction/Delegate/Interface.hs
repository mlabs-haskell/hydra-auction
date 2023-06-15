module HydraAuction.Delegate.Interface (
  DelegateResponse (..),
  DelegateState (..),
  InitializedState (..),
  InitializedStateKind (..),
  initializedStateKind,
  RequestIgnoredReason (..),
  ResponseReason (..),
  FrontendRequest (..),
  IncorrectRequestDataReason (..),
  ImposibleEvent (..),
  OpenHeadUtxo (..),
  MissingPrerequisite (..),
  AbortReason (..),
  initialState,
  isFinalState,
  wasOpened,
) where

-- Prelude imports
import Prelude

-- Haskell imports

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- Hydra imports
import Hydra.Cardano.Api (CtxUTxO, TxIn, TxOut)
import Hydra.Chain (HeadId)

-- HydraAuction imports
import HydraAuction.Types (AuctionTerms (..), BidTerms, StandingBidDatum)

data FrontendRequest
  = QueryCurrentDelegateState
  | CommitStandingBid
      { auctionTerms :: AuctionTerms
      , utxoToCommit :: (TxIn, TxOut CtxUTxO)
      }
  | NewBid
      { auctionTerms :: AuctionTerms
      , datum :: StandingBidDatum
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- |
   This is an important type.
   It should encode all Delegate server state required for its logic
   It should be (eventually) same for all servers, synced by Hydra events
   Client should have up-to date state, gotten by pushed responses.
-}
data DelegateState
  = NotInitialized
  | -- Hydra calls this Initializing.
    -- This case covers all Head states after it got Init comand
    -- and thus obtained HeadId.
    Initialized HeadId InitializedState
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

initialState :: DelegateState
initialState = NotInitialized

isFinalState :: DelegateState -> Bool
isFinalState (Initialized _ state) =
  state `elem` [Finalized, Aborted]
isFinalState NotInitialized = False

wasOpened :: DelegateState -> Bool
wasOpened state = case state of
  NotInitialized -> False
  Initialized _ (AwaitingCommits {}) -> False
  Initialized _ _ -> True

data OpenHeadUtxo = MkOpenHeadUtxo
  { standingBidTerms :: Maybe BidTerms
  , standingBidUtxo :: (TxIn, TxOut CtxUTxO)
  , -- Collateral of current delegate server
    collateralUtxo :: (TxIn, TxOut CtxUTxO)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data InitializedState
  = AwaitingCommits {stangingBidWasCommited :: Bool}
  | Open OpenHeadUtxo (Maybe AuctionTerms)
  | Closed
  | Finalized
  | AbortRequested AbortReason
  | Aborted
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data InitializedStateKind
  = AwaitingCommitsKind
  | OpenKind
  | ClosedKind
  | FinalizedKind
  | AbortRequestedKind
  | AbortedKind
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

initializedStateKind :: InitializedState -> InitializedStateKind
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

data RequestIgnoredReason
  = IncorrectRequestData IncorrectRequestDataReason
  | WrongDelegateState DelegateState
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- It is actually possible if one of Delegates is breaking protocol
data ImposibleEvent
  = -- FIXME: add docs and/or split on different cases
    IncorrectCommit
  | IncorrectStandingBidUtxoOnL2
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

data DelegateResponse
  = ClosingTxTemplate
  | CurrentDelegateState ResponseReason DelegateState
  | RequestIgnored RequestIgnoredReason
  | -- FIXME: possible duplication with CurrentDelegateState
    AuctionSet AuctionTerms
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
