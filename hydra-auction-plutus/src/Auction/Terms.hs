module Auction.Terms where

-- Prelude imports
import PlutusTx.Prelude
import Prelude qualified as Haskell

-- Haskell
import GHC.Generics (Generic)

-- Plutus imports
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusLedgerApi.V1.Value (AssetClass, CurrencySymbol)
import PlutusLedgerApi.V2.Contexts (TxOutRef)
import PlutusTx qualified

data AuctionTerms = AuctionTerms
  { at'AuctionLot :: AssetClass
  -- ^ NFT being sold in the auction.
  , at'SellerPKH :: PubKeyHash
  -- ^ Seller's pubkey hash, which will receive
  -- the proceeds of the auction (minus fees)
  -- if the auction lot is purchased,
  -- or reclaim the auction lot if it isn't.
  , at'SellerVK :: BuiltinByteString
  -- ^ Seller's verification key, used to control
  -- which bidders receive authorization to participate in the auction.
  , at'Delegates :: [PubKeyHash]
  -- ^ Group of delegates authorized to run the L2 bidding process.
  , at'BiddingStart :: POSIXTime
  -- ^ Start time of the bidding period.
  , at'BiddingEnd :: POSIXTime
  -- ^ End time of the bidding period.
  , at'PurchaseDeadline :: POSIXTime
  -- ^ Time by which the winning bidder can buy the auction lot.
  -- At and after this time, the winning bidder forfeits its bidder deposit
  -- if the auction lot has not been purchased.
  , at'Cleanup :: POSIXTime
  -- ^ Time at and after  which the remaining utxos in the auction
  -- can be unconditionally cleaned up, returning all tokens
  -- in those utxos to their original owners before the auction.
  , at'AuctionFeePerDelegate :: Integer
  -- ^ Fee portion that each delegate will receieve from
  -- the proceeds of the auction, whether the auction lot
  -- is purchased by a bidder or reclaimed by the seller.
  , at'StartingBid :: Integer
  -- ^ Bids cannot be lower than this number.
  , at'MinBidIncrement :: Integer
  -- ^ New bids can only supersede the standing bid if they exceed it
  -- by this increment.
  , at'MinDepositAmount :: Integer
  -- ^ Minimal amount of ADA that the seller requests
  -- each bidder to place as a bidder deposit for the auction.
  -- This is only enforced off-chain at the seller's discretion.
  }
  deriving stock (Generic, Haskell.Eq, Haskell.Show)
