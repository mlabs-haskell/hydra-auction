-- Temporary, until they will be used in scripts
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module HydraAuction.Types (AuctionTerms) where

import Prelude qualified

import Data.Natural (Natural)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Contexts (TxOutRef)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value (AssetClass)

-- Base datatypes

data AuctionTerms = AuctionTerms
  { -- | What is being sold at the auction?
    auctionLot :: AssetClass
  , -- | Who is selling it?
    seller :: PubKeyHash
  , -- | Who is running the Hydra Head where bidding occurs?
    delegates :: [PubKeyHash]
  , biddingStart :: POSIXTime
  , biddingEnd :: POSIXTime
  , voucherExpiry :: POSIXTime
  , -- | Auction lifecycle times.
    cleanup :: POSIXTime
  , -- | Total auction fee that will be evenly split among delegates.
    auctionFee :: Natural
  , -- | The auction lot cannot be sold for less than this bid price.
    startingBid :: Natural
  , -- | A new bid can only supersede the standing bid if it is larger
    -- by this increment.
    minimumBidIncrement :: Natural
  , -- | The seller consumed this utxo input in the transaction that
    -- announced this auction, to provide the auction lot to the auction.
    utxoRef :: TxOutRef
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

newtype ApprovedBidders = ApprovedBidders
  { -- | Which bidders are approved to submit bids?
    bidders :: [PubKeyHash]
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

data StandingBidState = NoBid | Bid BidTerms
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

data BidTerms = BidTerms
  { -- | Who submitted the bid?
    bidder :: PubKeyHash
  , -- | Which price did the bidder set to buy the auction lot?
    bidPrice :: Natural
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

data AuctionState
  = Announced
  | BiddingStarted ApprovedBiddersHash
  | Complete
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

-- TODO: String will be changed to actuall hash
newtype ApprovedBiddersHash = ApprovedBiddersHash Prelude.String
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
{- ^ This hash is calculated from the `ApprovedBidders` value that the seller
 fixes for the auction.
-}

-- Datums

newtype AuctionEscrowDatum = AuctionEscrowDatum
  { auctionState :: AuctionState
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

newtype StandingBidDatum = StandingBidDatum
  { standingBid :: StandingBidState
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

newtype BidDepositDatum = BidDepositDatum
  { -- | Which bidder made this deposit?
    bidder :: PubKeyHash
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

type AuctionFeeEscrowDatum = ()
-- ^ This datum is empty because the auction terms have all the required info.

type BidderMembershipDatum = ApprovedBidders
-- ^ The datum is just the list of approved bidders.
