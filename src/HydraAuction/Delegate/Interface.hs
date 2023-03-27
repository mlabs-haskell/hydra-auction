{-# LANGUAGE StrictData #-}

module HydraAuction.Delegate.Interface (
  DelegateResponse (..),
  FrontendRequest (..),
) where

-- Prelude imports
import Prelude

-- Haskell imports
import GHC.Generics (Generic)

-- Cardano imports
import Cardano.Api

-- HydraAuction imports
import HydraAuction.Types (AuctionTerms (..), Natural)

data FrontendRequest
  = CommitStandingBid
      { auctionTerms :: AuctionTerms
      , utxoToCommit :: TxIn
      }
  | -- FIXME: commit full datum
    NewBid {bidAmount :: Natural}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DelegateResponse
  = ClosingTxTemplate
  | HydraRequestError
  | AlreadyHasAuction
  | HasNoAuction
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
