{-# LANGUAGE StrictData #-}

module HydraAuction.Delegate.Interface (
  DelegateResponse (..),
  FrontendRequest (..),
) where

-- Prelude imports
import Prelude

-- Cardano imports
import Cardano.Api.UTxO (UTxO)

-- HydraAuction imports
import HydraAuction.Types (AuctionTerms (..), Natural)

data FrontendRequest
  = CommitStandingBid
      { auctionTerms :: AuctionTerms
      , utxoToCommit :: UTxO
      }
  | -- FIXME: commit full datum
    NewBid {bidAmount :: Natural}

data DelegateResponse
  = ClosingTxTemplate
  | HydraRequestError
  | AlreadyHasAuction
  | HasNoAuction
  deriving stock (Show)
