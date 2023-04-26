{-# OPTIONS_GHC -fno-specialise #-}
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
  BidDepositDatum (..),
  BidDepositRedeemer (..),
  StandingBidState (..),
  StandingBidDatum (..),
  AuctionTerms (..),
  AuctionState (..),
  ApprovedBidders (..),
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

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- Plutus imports
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol)
import Plutus.V2.Ledger.Contexts (TxOutRef)
import PlutusTx qualified
import PlutusTx.IsData.Class (FromData, ToData, UnsafeFromData)

-- Hydra auction imports
import HydraAuction.Addresses (VoucherCS)
import HydraAuctionUtils.Extras.PlutusOrphans ()
import HydraAuctionUtils.Types.Natural (Natural, intToNatural, naturalToInt)

-- Base datatypes
data AuctionStage
  = AnnouncedStage
  | BiddingStartedStage
  | BiddingEndedStage
  | VoucherExpiredStage
  | CleanupStage
  deriving stock
    (Prelude.Eq, Prelude.Ord, Prelude.Bounded, Prelude.Enum, Prelude.Show)

PlutusTx.makeIsDataIndexed
  ''AuctionStage
  [ ('AnnouncedStage, 0)
  , ('BiddingStartedStage, 1)
  , ('BiddingEndedStage, 2)
  , ('VoucherExpiredStage, 3)
  , ('CleanupStage, 4)
  ]
PlutusTx.makeLift ''AuctionStage

auctionStages :: [AuctionStage]
auctionStages = [Prelude.minBound .. Prelude.maxBound]

data AuctionTerms = AuctionTerms
  { auctionLot :: !AssetClass
  -- ^ What is being sold at the auction?
  , seller :: !PubKeyHash
  -- ^ Who is selling it?
  , hydraHeadId :: !CurrencySymbol
  -- ^ Which Hydra Head is authorized to host the bidding for this auction?
  , delegates :: ![PubKeyHash]
  -- ^ Who is running the Hydra Head where bidding occurs?
  , biddingStart :: !POSIXTime
  -- ^ Auction lifecycle times.
  , biddingEnd :: !POSIXTime
  , voucherExpiry :: !POSIXTime
  , cleanup :: !POSIXTime
  , auctionFeePerDelegate :: !Natural
  -- ^ Each delegate will receive this fee portion from the proceeds of
  -- the auction, when the auction lot is purchased or reclaimed.
  , startingBid :: !Natural
  -- ^ The auction lot cannot be sold for less than this bid price.
  , minimumBidIncrement :: !Natural
  -- ^ A new bid can only supersede the standing bid if it is larger
  -- by this increment.
  , utxoNonce :: !TxOutRef
  -- ^ The seller consumed this utxo input in the transaction that
  -- announced this auction, to provide the auction lot to the auction.
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq, Prelude.Ord)
  deriving anyclass (ToJSON, FromJSON)

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
  { bidders :: [PubKeyHash]
  -- ^ Which bidders are approved to submit bids?
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (ToJSON, FromJSON)

instance Eq ApprovedBidders where
  {-# INLINEABLE (==) #-}
  x == y = bidders x == bidders y

deriving newtype instance (UnsafeFromData ApprovedBidders)
deriving newtype instance (ToData ApprovedBidders)
deriving newtype instance (FromData ApprovedBidders)

PlutusTx.makeLift ''ApprovedBidders

data StandingBidState = StandingBidState
  { approvedBidders :: ApprovedBidders
  , standingBid :: Maybe BidTerms
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (ToJSON, FromJSON)

instance Eq StandingBidState where
  {-# INLINEABLE (==) #-}
  x == y = approvedBidders x == approvedBidders y && standingBid x == standingBid y

data BidTerms = BidTerms
  { bidBidder :: !PubKeyHash
  -- ^ Who submitted the bid?
  , bidAmount :: !Natural
  -- ^ Which amount did the bidder set to buy the auction lot?
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (FromJSON, ToJSON)

instance Eq BidTerms where
  {-# INLINEABLE (==) #-}
  x == y = (bidBidder x == bidBidder y) && (bidAmount x == bidAmount y)

PlutusTx.makeIsDataIndexed ''StandingBidState [('StandingBidState, 0)]
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
  deriving anyclass (ToJSON, FromJSON)

instance Eq StandingBidDatum where
  {-# INLINEABLE (==) #-}
  (StandingBidDatum x y) == (StandingBidDatum x' y') = (x == x') && (y == y')

PlutusTx.makeIsDataIndexed ''StandingBidDatum [('StandingBidDatum, 0)]
PlutusTx.makeLift ''StandingBidDatum

data BidDepositDatum = BidDepositDatum
  { bidDepositBidder :: !PubKeyHash
  -- ^ Which bidder made this deposit?
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

data BidDepositRedeemer = LosingBidder | WinningBidder | SellerClaimsDeposit | CleanupDeposit
PlutusTx.makeIsDataIndexed ''BidDepositRedeemer [('LosingBidder, 0), ('WinningBidder, 1), ('SellerClaimsDeposit, 2), ('CleanupDeposit, 3)]
