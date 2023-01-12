-- Temporary, until they will be used in scripts
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module HydraAuction.OnChain (AuctionTerms) where

import Prelude

import Data.Natural (Natural)
import Plutus.V1.Ledger.Contexts (TxOutRef)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value (AssetClass)

-- Base datatypes

data AuctionTerms = AuctionTerms
  { auctionLot :: AssetClass
  -- ^ What is being sold at the auction?
  , seller     :: PubKeyHash
  -- ^ Who is selling it?
  , delegates :: [PubKeyHash]
  -- ^ Who is running the Hydra Head where bidding occurs?
  , biddingStart :: POSIXTime
  , biddingEnd :: POSIXTime
  , voucherExpiry :: POSIXTime
  , cleanup :: POSIXTime
  -- ^ Auction lifecycle times.
  , auctionFee :: Natural
  -- ^ Total auction fee that will be evenly split among delegates.
  , startingBid :: Natural
  -- ^ The auction lot cannot be sold for less than this bid price.
  , minimumBidIncrement :: Natural
  -- ^ A new bid can only supersede the standing bid if it is larger
  -- by this increment.
  , utxoRef :: TxOutRef
  -- ^ The seller consumed this utxo input in the transaction that
  -- announced this auction, to provide the auction lot to the auction.
  }

data ApprovedBidders = ApprovedBidders
  { bidders :: [PubKeyHash]
  -- ^ Which bidders are approved to submit bids?
  }

data StandingBidState = NoBid | Bid BidTerms

data BidTerms = BidTerms
  { bidder :: PubKeyHash
  -- ^ Who submitted the bid?
  , bidPrice :: Natural
  -- ^ Which price did the bidder set to buy the auction lot?
  }

data AuctionState =
    Announced
  | BiddingStarted ApprovedBiddersHash
  | Complete

-- TODO: String will be changed to actuall hash
data ApprovedBiddersHash = ApprovedBiddersHash String
-- ^ This hash is calculated from the `ApprovedBidders` value that the seller
-- fixes for the auction.

-- Datums

data AuctionEscrowDatum = AuctionEscrowDatum
  { auctionState :: AuctionState
  }

data StandingBidDatum = StandingBidDatum
  { standingBid :: StandingBidState
  }

data BidDepositDatum = BidDepositDatum
  { bidder :: PubKeyHash
  -- ^ Which bidder made this deposit?
  }

type AuctionFeeEscrowDatum = ()
-- ^ This datum is empty because the auction terms have all the required info.

type BidderMembershipDatum = ApprovedBidders
-- ^ The datum is just the list of approved bidders.
