{-# LANGUAGE StrictData #-}

module HydraAuction.Delegate.Interface (
  DelegateResponse (..),
  DelegateState (..),
  InitializedState (..),
  RequestIgnoredReason (..),
  ResponseReason (..),
  FrontendRequest (..),
  initialState,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import GHC.Generics (Generic)

-- Cardano imports
import Cardano.Api

-- Hydra imports
import Hydra.Chain (HeadId)

-- HydraAuction imports
import HydraAuction.Types (AuctionTerms (..), Natural)

data FrontendRequest
  = -- FIXME: handle new client
    QueryCurrentDelegateState
  | CommitStandingBid
      { auctionTerms :: AuctionTerms
      , utxoToCommit :: TxIn
      }
  | -- FIXME: commit full datum
    NewBid {bidAmount :: Natural}
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

data InitializedState
  = NotYetOpen
  | -- FIXME: fix stanging bid address here?
    HasCommit
  | Open
      { -- FIXME: other delegates may not know AuctionTerms, only standing bid
        standingBidAmount :: Maybe Natural
      }
  | Closed
  | Finalized
  -- FIXME: add Aborted
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RequestIgnoredReason
  = IncorrectData -- FIXME: add specifics?
  | WrongDelegateState DelegateState
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ResponseReason = Greeting | WasQueried | Updated
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
