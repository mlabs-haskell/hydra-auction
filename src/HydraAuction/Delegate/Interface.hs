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
import HydraAuction.Types (AuctionTerms (..), BidTerms, StandingBidDatum)
import Prettyprinter (Pretty (pretty), indent, viaShow, vsep, (<+>))

data FrontendRequest
  = -- FIXME: handle new client
    QueryCurrentDelegateState
  | CommitStandingBid
      { auctionTerms :: AuctionTerms
      , utxoToCommit :: TxIn
      }
  | -- FIXME: commit full datum
    NewBid
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

data InitializedState
  = NotYetOpen
  | -- FIXME: fix stanging bid address here?
    HasCommit
  | Open
      { standingBidTerms :: Maybe BidTerms
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

instance Pretty RequestIgnoredReason where
  pretty = \case
    IncorrectData -> "data was incorrect"
    WrongDelegateState state ->
      vsep
        [ "The delegate in current delegate state could not process request"
        , indent 2 $ viaShow state
        ]

data ResponseReason = Greeting | WasQueried | Updated
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty ResponseReason where
  pretty = \case
    Greeting -> "delegate sends a greeting"
    WasQueried -> "delegate was queried"
    Updated -> "delegate state was updated"

data DelegateResponse
  = ClosingTxTemplate
  | CurrentDelegateState ResponseReason DelegateState
  | RequestIgnored RequestIgnoredReason
  | -- FIXME: possible duplication with CurrentDelegateState
    AuctionSet AuctionTerms
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty DelegateResponse where
  pretty = \case
    ClosingTxTemplate -> "ClosingTxTemplate"
    RequestIgnored reason ->
      vsep
        [ "The request was ignored because"
        , indent 2 $ pretty reason
        ]
    CurrentDelegateState reason state ->
      "Delegate State Answer"
        <> indent
          2
          ( vsep
              [ "Responding because" <+> pretty reason
              , -- FIXME: proper implementation for pretty printing the
                -- DelegateState
                "Current state:" <+> viaShow state
              ]
          )
    AuctionSet terms ->
      vsep
        [ "Auction set"
        , indent 2 $ viaShow terms
        ]
