module HydraAuction.Delegate.Interface (
  FrontendKind (..),
  KnownFrontendRequest (..),
  DelegateError (..),
  DelegateResponse (..),
  FrontendRequest (..),
) where

-- Prelude imports
import Prelude

-- Cardano imports
import Cardano.Api.UTxO (UTxO)

-- HydraAuction imports
import HydraAuction.Types (AuctionTerms (..), Natural)

data FrontendKind = Seller | Bidder
data KnownFrontendRequest
  = CommitStandingBid UTxO
  | NewBid Natural

data FrontendRequest
  = FrontendConnect AuctionTerms FrontendKind
  | KnownFrontendRequest KnownFrontendRequest

data DelegateError
  = NoClientYet
  | WrongClientSignature
  | WrongClientKind
  | HydraRequestError
  deriving stock (Show)

data DelegateResponse = Okay | ClosingPreparedTransaction
  deriving stock (Show)
