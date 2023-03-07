module HydraAuction.Delegate.Interface (
  FrontendKind (..),
  KnownFrontendRequest (..),
  DelegateError (..),
  DelegateResponse (..),
  FrontendRequest (..),
) where

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
data DelegateResponse = Okay | ClosingPreparedTransaction
