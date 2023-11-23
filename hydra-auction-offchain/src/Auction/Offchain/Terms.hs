module Auction.Offchain.Terms where

import Prelude

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

import Cardano.Api.Shelley (
  AssetId (..),
  Hash (..),
  Key (..),
  Lovelace (..),
  PaymentKey,
 )

data AuctionTerms = AuctionTerms
  { at'AuctionLot :: AssetId
  -- ^ NFT being sold in the auction.
  , at'SellerPKH :: Hash PaymentKey
  -- ^ Seller's pubkey hash, which will receive
  -- the proceeds of the auction (minus fees)
  -- if the auction lot is purchased,
  -- or reclaim the auction lot if it isn't.
  , at'SellerVK :: VerificationKey PaymentKey
  -- ^ Seller's verification key, used to control
  -- which bidders receive authorization to participate in the auction.
  , at'Delegates :: [Hash PaymentKey]
  -- ^ Group of delegates authorized to run the L2 bidding process.
  , at'BiddingStart :: UTCTime
  -- ^ Start time of the bidding period.
  , at'BiddingEnd :: UTCTime
  -- ^ End time of the bidding period.
  , at'PurchaseDeadline :: UTCTime
  -- ^ Time by which the winning bidder can buy the auction lot.
  -- At and after this time, the winning bidder forfeits its bidder deposit
  -- if the auction lot has not been purchased.
  , at'Cleanup :: UTCTime
  -- ^ Time at and after  which the remaining utxos in the auction
  -- can be unconditionally cleaned up, returning all tokens
  -- in those utxos to their original owners before the auction.
  , at'AuctionFeePerDelegate :: Lovelace
  -- ^ Fee portion that each delegate will receieve from
  -- the proceeds of the auction, whether the auction lot
  -- is purchased by a bidder or reclaimed by the seller.
  , at'StartingBid :: Lovelace
  -- ^ Bids cannot be lower than this number.
  , at'MinBidIncrement :: Lovelace
  -- ^ New bids can only supersede the standing bid if they exceed it
  -- by this increment.
  , at'MinDepositAmount :: Lovelace
  -- ^ Minimal amount of ADA that the seller requests
  -- each bidder to place as a bidder deposit for the auction.
  -- This is only enforced off-chain at the seller's discretion.
  }
  deriving stock (Generic, Eq, Show)
