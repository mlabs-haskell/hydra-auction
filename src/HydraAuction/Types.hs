{-# OPTIONS -Wno-orphans #-}
module HydraAuction.Types (
  auctionState,
  standingBid,
  isStarted,
  StandingBidState (..),
  StandingBidDatum (..),
  AuctionTerms (..),
  AuctionState (..),
  AuctionEscrowDatum (..),
  AuctionFeeEscrowDatum,
  BidderMembershipDatum,
) where

import Prelude qualified

import GHC.Generics (Generic)
import Plutus.V1.Ledger.Contexts (TxOutRef)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol)
import PlutusTx qualified
import PlutusTx.IsData.Class (FromData (fromBuiltinData), ToData (toBuiltinData), UnsafeFromData (unsafeFromBuiltinData))
import PlutusTx.Prelude hiding (fromInteger)
import PlutusTx.Prelude qualified as Plutus

newtype Natural = Natural Integer
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance UnsafeFromData Natural where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = fromJust . intToNatural . unsafeFromBuiltinData

instance ToData Natural where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = toBuiltinData . naturalToInt

instance FromData Natural where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d = do
    i <- fromBuiltinData d
    intToNatural i

{-# INLINEABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust _ = error ()

{-# INLINEABLE intToNatural #-}
intToNatural :: Integer -> Maybe Natural
intToNatural x
  | x > 0 = Just $ Natural x
  | otherwise = Nothing

{-# INLINEABLE naturalToInt #-}
naturalToInt :: Natural -> Integer
naturalToInt (Natural i) = i

PlutusTx.makeLift ''Natural

-- FIXME: Use template Haskell to derive Eq instances
instance Eq Natural where
  {-# INLINEABLE (==) #-}
  x == y = naturalToInt x == naturalToInt y

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
    auctionFee :: Integer
  , -- | The auction lot cannot be sold for less than this bid price.
    startingBid :: Integer
  , -- | A new bid can only supersede the standing bid if it is larger
    -- by this increment.
    minimumBidIncrement :: Integer
  , -- | The seller consumed this utxo input in the transaction that
    -- announced this auction, to provide the auction lot to the auction.
    utxoRef :: TxOutRef
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

PlutusTx.makeIsDataIndexed ''AuctionTerms [('AuctionTerms, 0)]
PlutusTx.makeLift ''AuctionTerms

instance Eq AuctionTerms where
  {-# INLINEABLE (==) #-}
  x == y =
    (auctionLot x == auctionLot y)
      && (seller x == seller y)
      && (delegates x == delegates y)
      && (biddingStart x == biddingStart y)
      && (biddingEnd x == biddingEnd y)
      && (voucherExpiry x == voucherExpiry y)
      && (cleanup x == cleanup y)
      && (auctionFee x == auctionFee y)
      && (startingBid x == startingBid y)
      && (minimumBidIncrement x == minimumBidIncrement y)
      && (utxoRef x == utxoRef y)

newtype ApprovedBidders = ApprovedBidders
  { -- | Which bidders are approved to submit bids?
    bidders :: [PubKeyHash]
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Eq ApprovedBidders where
  {-# INLINEABLE (==) #-}
  x == y = bidders x == bidders y

PlutusTx.makeIsDataIndexed ''ApprovedBidders [('ApprovedBidders, 0)]
PlutusTx.makeLift ''ApprovedBidders

data StandingBidState = NoBid | Bid BidTerms
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Eq StandingBidState where
  {-# INLINEABLE (==) #-}
  NoBid == NoBid = True
  (Bid x) == (Bid y) = x == y
  _ == _ = False

data BidTerms = BidTerms
  { -- | Who submitted the bid?
    bidder :: PubKeyHash
  , -- | Which price did the bidder set to buy the auction lot?
    bidPrice :: Integer
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Eq BidTerms where
  {-# INLINEABLE (==) #-}
  x == y = (bidder x == bidder y) && (bidPrice x == bidPrice y)

PlutusTx.makeIsDataIndexed ''StandingBidState [('NoBid, 0), ('Bid, 1)]
PlutusTx.makeLift ''StandingBidState
PlutusTx.makeIsDataIndexed ''BidTerms [('BidTerms, 0)]
PlutusTx.makeLift ''BidTerms

data AuctionState
  = Announced
  | BiddingStarted ApprovedBiddersHash
  | Complete
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

{-# INLINEABLE isStarted #-}
isStarted :: AuctionState -> Bool
isStarted (BiddingStarted _) = True
isStarted _ = False

instance Eq AuctionState where
  {-# INLINEABLE (==) #-}
  Announced == Announced = True
  Complete == Complete = True
  (BiddingStarted x) == (BiddingStarted y) = x == y
  _ == _ = False

-- | TODO: Bytetring will be changed to actuall hash
newtype ApprovedBiddersHash = ApprovedBiddersHash Plutus.BuiltinByteString
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
{- ^ This hash is calculated from the `ApprovedBidders` value that the seller
 fixes for the auction.
-}

instance Eq ApprovedBiddersHash where
  {-# INLINEABLE (==) #-}
  (ApprovedBiddersHash x) == (ApprovedBiddersHash y) = x == y

PlutusTx.makeIsDataIndexed ''AuctionState [('Announced, 0), ('Complete, 1), ('BiddingStarted, 2)]
PlutusTx.makeLift ''AuctionState
PlutusTx.makeIsDataIndexed ''ApprovedBiddersHash [('ApprovedBiddersHash, 0)]
PlutusTx.makeLift ''ApprovedBiddersHash

-- Datums

data AuctionEscrowDatum = AuctionEscrowDatum AuctionState CurrencySymbol
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

{-# INLINEABLE auctionState #-}
auctionState :: AuctionEscrowDatum -> AuctionState
auctionState (AuctionEscrowDatum x _) = x

instance Eq AuctionEscrowDatum where
  {-# INLINEABLE (==) #-}
  (AuctionEscrowDatum x x') == (AuctionEscrowDatum y y') = (x == y) && (x' == y')

PlutusTx.makeIsDataIndexed ''AuctionEscrowDatum [('AuctionEscrowDatum, 0)]
PlutusTx.makeLift ''AuctionEscrowDatum

newtype StandingBidDatum = StandingBidDatum StandingBidState
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

{-# INLINEABLE standingBid #-}
standingBid :: StandingBidDatum -> StandingBidState
standingBid (StandingBidDatum x) = x

instance Eq StandingBidDatum where
  {-# INLINEABLE (==) #-}
  (StandingBidDatum x) == (StandingBidDatum y) = x == y

PlutusTx.makeIsDataIndexed ''StandingBidDatum [('StandingBidDatum, 0)]
PlutusTx.makeLift ''StandingBidDatum

newtype BidDepositDatum = BidDepositDatum
  { -- | Which bidder made this deposit?
    bidder :: PubKeyHash
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Eq BidDepositDatum where
  {-# INLINEABLE (==) #-}
  (BidDepositDatum x) == (BidDepositDatum y) = x == y

PlutusTx.makeIsDataIndexed ''BidDepositDatum [('BidDepositDatum, 0)]
PlutusTx.makeLift ''BidDepositDatum

type AuctionFeeEscrowDatum = ()
-- ^ This datum is empty because the auction terms have all the required info.

type BidderMembershipDatum = ApprovedBidders
-- ^ The datum is just the list of approved bidders.
