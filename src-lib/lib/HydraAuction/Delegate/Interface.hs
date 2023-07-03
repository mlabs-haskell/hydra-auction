-- | Bunch of generic stuff is reexported from `HydraAuctionUtils.Delegate`
module HydraAuction.Delegate.Interface (
  DelegateResponse (..),
  DelegateState (..),
  HydraHeadInfo (..),
  DelegateProtocol,
  DelegateLogicTypes (..),
  CommitAction (..),
  CustomEvent (..),
  TxAction (..),
  OpenState (..),
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
  wasStopped,
) where

-- Prelude imports
import Prelude

-- Haskell imports

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- Hydra imports
import Hydra.Cardano.Api (CtxUTxO, TxIn, TxOut)

-- HydraAuction imports

import HydraAuction.Types (
  AuctionStage,
  AuctionTerms (..),
  BidTerms,
  StandingBidDatum,
 )
import HydraAuctionUtils.Delegate.Interface
import HydraAuctionUtils.Server.Protocol (Protocol (..))

data DelegateProtocol

data DelegateResponseKind = NotImplemented deriving stock (Eq, Show)

instance Protocol DelegateProtocol where
  type Input DelegateProtocol = FrontendRequest DelegateProtocol
  type Output DelegateProtocol = DelegateResponse DelegateProtocol
  type OutputKind DelegateProtocol = DelegateResponseKind
  type ConnectionConfig DelegateProtocol = ()
  getOutputKind _ = NotImplemented
  configToConnectionPath _ () = ""

instance DelegateLogicTypes DelegateProtocol where
  data CommitAction DelegateProtocol = MoveStandingBidToL2
    { commitAuctionTerms :: AuctionTerms
    , utxoToCommit :: (TxIn, TxOut CtxUTxO)
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

  data TxAction DelegateProtocol = NewBid
    { txAuctionTerms :: AuctionTerms
    , datum :: StandingBidDatum
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

  data OpenState DelegateProtocol
    = MkOpenState OpenHeadUtxo (Maybe AuctionTerms)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

  data CustomEvent DelegateProtocol
    = AuctionSet AuctionTerms
    | AuctionStageStarted AuctionTerms AuctionStage
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data OpenHeadUtxo = MkOpenHeadUtxo
  { standingBidTerms :: Maybe BidTerms
  , standingBidUtxo :: (TxIn, TxOut CtxUTxO)
  , -- Collateral of current delegate server
    collateralUtxo :: (TxIn, TxOut CtxUTxO)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
