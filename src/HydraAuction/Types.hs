{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

-- FIXME: Use template Haskell to derive Eq instances
module HydraAuction.Types (
  isStarted,
  intToNatural,
  naturalToInt,
  Natural,
  ApprovedBiddersHash (..),
  BidTerms (..),
  StandingBidState (..),
  StandingBidDatum (..),
  AuctionTerms (..),
  AuctionState (..),
  AuctionEscrowDatum (..),
  EscrowRedeemer (..),
  StandingBidRedeemer (..),
  AuctionFeeEscrowDatum,
  VoucherForgingRedeemer (..),
  calculateTotalFee,
  AuctionStage (..),
  auctionStages,
) where

-- Prelude imports

import PlutusTx.Prelude
import Prelude qualified

-- Haskell imports
import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), ToJSON)
import GHC.Generics (Generic)

-- Plutus imports
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol)
import Plutus.V2.Ledger.Contexts (TxOutRef)
import PlutusTx qualified
import PlutusTx.IsData.Class (FromData (fromBuiltinData), ToData (toBuiltinData), UnsafeFromData (unsafeFromBuiltinData))

-- Hydra auction imports
import HydraAuction.Addresses (VoucherCS)

-- Custom Natural

-- Not using natural-numbers package, cuz it does not provide Integer conversion methods,
-- which are compatible with PlutusTx

newtype Natural = Natural Integer
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving newtype (Eq, Ord, AdditiveSemigroup, MultiplicativeSemigroup, ToJSON)

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

instance FromJSON Natural where
  parseJSON x = do
    int <- parseJSON x
    case intToNatural int of
      Just nat -> return nat
      Nothing -> fail "Integer is not natural"

-- Base datatypes

data AuctionStage
  = AnnouncedStage
  | BiddingStartedStage
  | BiddingEndedStage
  | VoucherExpiredStage
  deriving stock
    (Prelude.Eq, Prelude.Ord, Prelude.Bounded, Prelude.Enum, Prelude.Show)

PlutusTx.makeIsDataIndexed
  ''AuctionStage
  [ ('AnnouncedStage, 0)
  , ('BiddingStartedStage, 1)
  , ('BiddingEndedStage, 2)
  , ('VoucherExpiredStage, 3)
  ]
PlutusTx.makeLift ''AuctionStage

auctionStages :: [AuctionStage]
auctionStages = [Prelude.minBound .. Prelude.maxBound]

data AuctionTerms = AuctionTerms
  { -- | What is being sold at the auction?
    auctionLot :: !AssetClass
  , -- | Who is selling it?
    seller :: !PubKeyHash
  , -- | Which Hydra Head is authorized to host the bidding for this auction?
    hydraHeadId :: !CurrencySymbol
  , -- | Who is running the Hydra Head where bidding occurs?
    delegates :: ![PubKeyHash]
  , -- | Auction lifecycle times.
    biddingStart :: !POSIXTime
  , biddingEnd :: !POSIXTime
  , voucherExpiry :: !POSIXTime
  , cleanup :: !POSIXTime
  , -- | Each delegate will receive this fee portion from the proceeds of
    -- the auction, when the auction lot is purchased or reclaimed.
    auctionFeePerDelegate :: !Natural
  , -- | The auction lot cannot be sold for less than this bid price.
    startingBid :: !Natural
  , -- | A new bid can only supersede the standing bid if it is larger
    -- by this increment.
    minimumBidIncrement :: !Natural
  , -- | The seller consumed this utxo input in the transaction that
    -- announced this auction, to provide the auction lot to the auction.
    utxoNonce :: !TxOutRef
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
      && (auctionFeePerDelegate x == auctionFeePerDelegate y)
      && (startingBid x == startingBid y)
      && (minimumBidIncrement x == minimumBidIncrement y)
      && (utxoNonce x == utxoNonce y)

calculateTotalFee :: AuctionTerms -> Integer
calculateTotalFee terms =
  naturalToInt (auctionFeePerDelegate terms) * length (delegates terms)

newtype ApprovedBidders = ApprovedBidders
  { -- | Which bidders are approved to submit bids?
    bidders :: [PubKeyHash]
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Eq ApprovedBidders where
  {-# INLINEABLE (==) #-}
  x == y = bidders x == bidders y

deriving newtype instance (UnsafeFromData ApprovedBidders)
deriving newtype instance (ToData ApprovedBidders)
deriving newtype instance (FromData ApprovedBidders)

PlutusTx.makeLift ''ApprovedBidders

data StandingBidState = NoBid | Bid !BidTerms
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Eq StandingBidState where
  {-# INLINEABLE (==) #-}
  NoBid == NoBid = True
  (Bid x) == (Bid y) = x == y
  _ == _ = False

data BidTerms = BidTerms
  { -- | Who submitted the bid?
    bidBidder :: !PubKeyHash
  , -- | Which amount did the bidder set to buy the auction lot?
    bidAmount :: !Natural
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Eq BidTerms where
  {-# INLINEABLE (==) #-}
  x == y = (bidBidder x == bidBidder y) && (bidAmount x == bidAmount y)

PlutusTx.makeIsDataIndexed ''StandingBidState [('NoBid, 0), ('Bid, 1)]
PlutusTx.makeLift ''StandingBidState
PlutusTx.makeIsDataIndexed ''BidTerms [('BidTerms, 0)]
PlutusTx.makeLift ''BidTerms

data AuctionState
  = Announced
  | BiddingStarted !ApprovedBiddersHash
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

{-# INLINEABLE isStarted #-}
isStarted :: AuctionState -> Bool
isStarted (BiddingStarted _) = True
isStarted _ = False

instance Eq AuctionState where
  {-# INLINEABLE (==) #-}
  Announced == Announced = True
  (BiddingStarted x) == (BiddingStarted y) = x == y
  _ == _ = False

-- | FIXME: Bytetring will be changed to actual hash
newtype ApprovedBiddersHash = ApprovedBiddersHash BuiltinByteString
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
{- ^ This hash is calculated from the `ApprovedBidders` value that the seller
 fixes for the auction.
-}

deriving newtype instance (UnsafeFromData ApprovedBiddersHash)
deriving newtype instance (ToData ApprovedBiddersHash)
deriving newtype instance (FromData ApprovedBiddersHash)
PlutusTx.makeLift ''ApprovedBiddersHash

instance Eq ApprovedBiddersHash where
  {-# INLINEABLE (==) #-}
  (ApprovedBiddersHash x) == (ApprovedBiddersHash y) = x == y

PlutusTx.makeIsDataIndexed ''AuctionState [('Announced, 0), ('BiddingStarted, 1)]
PlutusTx.makeLift ''AuctionState

-- Datums

data AuctionEscrowDatum = AuctionEscrowDatum
  { auctionState :: !AuctionState
  , auctionVoucherCS :: !VoucherCS
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Eq AuctionEscrowDatum where
  {-# INLINEABLE (==) #-}
  (AuctionEscrowDatum x x') == (AuctionEscrowDatum y y') = (x == y) && (x' == y')

PlutusTx.makeIsDataIndexed ''AuctionEscrowDatum [('AuctionEscrowDatum, 0)]
PlutusTx.makeLift ''AuctionEscrowDatum

data StandingBidDatum = StandingBidDatum
  { standingBidState :: !StandingBidState
  , standingBidVoucherCS :: !VoucherCS
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Eq StandingBidDatum where
  {-# INLINEABLE (==) #-}
  (StandingBidDatum x y) == (StandingBidDatum x' y') = (x == x') && (y == y')

PlutusTx.makeIsDataIndexed ''StandingBidDatum [('StandingBidDatum, 0)]
PlutusTx.makeLift ''StandingBidDatum

data BidDepositDatum = BidDepositDatum
  { -- | Which bidder made this deposit?
    bidDepositBidder :: !PubKeyHash
  , bidDepositVoucherCS :: !VoucherCS
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Eq BidDepositDatum where
  {-# INLINEABLE (==) #-}
  (BidDepositDatum x y) == (BidDepositDatum x' y') = (x == x') && (y == y')

PlutusTx.makeIsDataIndexed ''BidDepositDatum [('BidDepositDatum, 0)]
PlutusTx.makeLift ''BidDepositDatum

type AuctionFeeEscrowDatum = ()

-- Redeemers

data EscrowRedeemer = StartBidding | SellerReclaims | BidderBuys
PlutusTx.makeIsDataIndexed ''EscrowRedeemer [('StartBidding, 0), ('SellerReclaims, 1), ('BidderBuys, 2)]

data StandingBidRedeemer = MoveToHydra | NewBid | Cleanup
PlutusTx.makeIsDataIndexed ''StandingBidRedeemer [('MoveToHydra, 0), ('NewBid, 1), ('Cleanup, 2)]

data VoucherForgingRedeemer = MintVoucher | BurnVoucher
PlutusTx.makeIsDataIndexed ''VoucherForgingRedeemer [('MintVoucher, 0), ('BurnVoucher, 1)]

data BidDepositRedeemer = LosingBidder | WinningBidder | SellerClaimsDeposit
PlutusTx.makeIsDataIndexed ''BidDepositRedeemer [('LosingBidder, 0), ('WinningBidder, 1), ('SellerClaimsDeposit, 2)]
