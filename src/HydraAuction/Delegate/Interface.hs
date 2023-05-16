{-# LANGUAGE StrictData #-}

module HydraAuction.Delegate.Interface (
  DelegateResponse (..),
  DelegateState (..),
  InitializedState (..),
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
      , utxoToCommit :: TxIn
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
  Initialized _ NotYetOpen -> False
  Initialized _ (HasCommit {}) -> False
  Initialized _ _ -> True

data InitializedState
  = NotYetOpen
  | -- FIXME: fix stanging bid address here?
    HasCommit
  | Open
      { standingBidTerms :: Maybe BidTerms
      }
  | Closed
  | Finalized
  | AbortRequested AbortReason
  | Aborted
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

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

data ImposibleEvent
  = -- FIXME: actually it is not imposible if one of Delegates is malignant
    -- FIXME: add docs and/or split on different cases
    IncorrectStandingBidUtxoOnL2
  | OnChainInvariantBreaks
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data MissingPrerequisite = AdaForCommit
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
