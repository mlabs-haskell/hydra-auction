module HydraAuction.Types (
  exampleTerms,
  isStarted,
  BidTerms (..),
  BidDepositDatum (..),
  BidDepositRedeemer (..),
  StandingBidState (..),
  StandingBidDatum (..),
  AuctionTerms (..),
  AuctionState (..),
  AuctionEscrowDatum (..),
  EscrowRedeemer (..),
  StandingBidRedeemer (..),
  FeeEscrowDatum,
  FeeEscrowRedeemer (..),
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
import Data.Maybe (fromJust)
import Data.String (IsString (..))
import GHC.Generics (Generic)

-- Plutus imports
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusLedgerApi.V1.Value (AssetClass, CurrencySymbol)
import PlutusLedgerApi.V2.Contexts (TxOutRef)
import PlutusTx qualified
import PlutusTx.Deriving qualified as PlutusTx

-- Hydra auction imports
import Hydra.Cardano.Api (
  PaymentKey,
  VerificationKey,
  serialiseToRawBytes,
  toPlutusKeyHash,
  toPlutusTxOutRef,
  verificationKeyHash,
  pattern TxIn,
  pattern TxIx,
 )
import HydraAuction.Addresses (VoucherCS)
import HydraAuction.OnChain.TestNFT (testNftAssetClass, testNftCurrencySymbol)
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
    ( Prelude.Eq
    , Prelude.Ord
    , Prelude.Bounded
    , Prelude.Enum
    , Prelude.Show
    , Generic
    )
  deriving anyclass (FromJSON, ToJSON)

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
  { auctionLot :: AssetClass
  -- ^ What is being sold at the auction?
  , sellerPKH :: PubKeyHash
  -- ^ Who is selling it?
  , sellerVK :: BuiltinByteString
  -- ^ Verification key of the seller
  , hydraHeadId :: CurrencySymbol
  -- ^ Which Hydra Head is authorized to host the bidding for this auction?
  , delegates :: [PubKeyHash]
  -- ^ Who is running the Hydra Head where bidding occurs?
  , biddingStart :: POSIXTime
  -- ^ Auction lifecycle times.
  , biddingEnd :: POSIXTime
  , voucherExpiry :: POSIXTime
  , cleanup :: POSIXTime
  , auctionFeePerDelegate :: Natural
  -- ^ Each delegate will receive this fee portion from the proceeds of
  -- the auction, when the auction lot is purchased or reclaimed.
  , startingBid :: Natural
  -- ^ The auction lot cannot be sold for less than this bid price.
  , minimumBidIncrement :: Natural
  -- ^ A new bid can only supersede the standing bid if it is larger
  -- by this increment.
  , utxoNonce :: TxOutRef
  -- ^ The seller consumed this utxo input in the transaction that
  -- announced this auction, to provide the auction lot to the auction.
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq, Prelude.Ord)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''AuctionTerms [('AuctionTerms, 0)]
PlutusTx.makeLift ''AuctionTerms
PlutusTx.deriveEq ''AuctionTerms

-- | Should not differ between the versions
exampleTerms :: VerificationKey PaymentKey -> AuctionTerms
exampleTerms sellerVk =
  let
   in AuctionTerms
        { auctionLot = testNftAssetClass
        , sellerPKH = toPlutusKeyHash $ verificationKeyHash sellerVk
        , sellerVK = toBuiltin $ serialiseToRawBytes sellerVk
        , hydraHeadId = testNftCurrencySymbol
        , delegates = []
        , biddingStart = POSIXTime 0
        , biddingEnd = POSIXTime 1
        , voucherExpiry = POSIXTime 2
        , cleanup = POSIXTime 3
        , auctionFeePerDelegate = fromJust $ intToNatural 4000000
        , startingBid = fromJust $ intToNatural 1
        , minimumBidIncrement = fromJust $ intToNatural 1
        , utxoNonce =
            toPlutusTxOutRef $
              TxIn
                ( fromString $
                    "d68eeb8a86479fa6e173211203914e4601"
                      <> "fead46f6ed711c4036daa69d095f6f"
                )
                (TxIx 0)
        }

calculateTotalFee :: AuctionTerms -> Integer
calculateTotalFee terms =
  naturalToInt (auctionFeePerDelegate terms) * length (delegates terms)

newtype StandingBidState = StandingBidState {standingBid :: Maybe BidTerms}
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (ToJSON, FromJSON)

data BidTerms = BidTerms
  { bidderPKH :: PubKeyHash
  -- ^ PubKeyHash of whoever submitted the bid?
  , bidderVK :: BuiltinByteString
  -- ^ Verification Key of whoever submitted the bid
  , bidAmount :: Natural
  -- ^ Which amount did the bidder set to buy the auction lot?
  , bidderSignature :: BuiltinByteString
  -- ^ Represents the signed payload by the bidder
  , sellerSignature :: BuiltinByteString
  -- ^ Represents the signed payload by the seller
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''BidTerms [('BidTerms, 0)]
PlutusTx.makeLift ''BidTerms
PlutusTx.deriveEq ''BidTerms
PlutusTx.makeIsDataIndexed ''StandingBidState [('StandingBidState, 0)]
PlutusTx.makeLift ''StandingBidState
PlutusTx.deriveEq ''StandingBidState

data AuctionState
  = Announced
  | BiddingStarted
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

{-# INLINEABLE isStarted #-}
isStarted :: AuctionState -> Bool
isStarted BiddingStarted = True
isStarted _ = False

PlutusTx.deriveEq ''AuctionState
PlutusTx.makeIsDataIndexed ''AuctionState [('Announced, 0), ('BiddingStarted, 1)]
PlutusTx.makeLift ''AuctionState

-- Datums

data AuctionEscrowDatum = AuctionEscrowDatum
  { auctionState :: AuctionState
  , auctionVoucherCS :: VoucherCS
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

PlutusTx.deriveEq ''AuctionEscrowDatum
PlutusTx.makeIsDataIndexed ''AuctionEscrowDatum [('AuctionEscrowDatum, 0)]
PlutusTx.makeLift ''AuctionEscrowDatum

data StandingBidDatum = StandingBidDatum
  { standingBidState :: StandingBidState
  , standingBidVoucherCS :: VoucherCS
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.deriveEq ''StandingBidDatum
PlutusTx.makeIsDataIndexed ''StandingBidDatum [('StandingBidDatum, 0)]
PlutusTx.makeLift ''StandingBidDatum

data BidDepositDatum = BidDepositDatum
  { bidDepositBidder :: PubKeyHash
  -- ^ Which bidder made this deposit?
  , bidDepositVoucherCS :: VoucherCS
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

PlutusTx.deriveEq ''BidDepositDatum
PlutusTx.makeIsDataIndexed ''BidDepositDatum [('BidDepositDatum, 0)]
PlutusTx.makeLift ''BidDepositDatum

type FeeEscrowDatum = ()

-- Redeemers

data EscrowRedeemer = StartBidding | SellerReclaims | BidderBuys
PlutusTx.makeIsDataIndexed ''EscrowRedeemer [('StartBidding, 0), ('SellerReclaims, 1), ('BidderBuys, 2)]

data StandingBidRedeemer = MoveToHydra | NewBid | Cleanup
PlutusTx.makeIsDataIndexed ''StandingBidRedeemer [('MoveToHydra, 0), ('NewBid, 1), ('Cleanup, 2)]

data VoucherForgingRedeemer = MintVoucher | BurnVoucher
PlutusTx.makeIsDataIndexed ''VoucherForgingRedeemer [('MintVoucher, 0), ('BurnVoucher, 1)]

data BidDepositRedeemer = LosingBidder | WinningBidder | SellerClaimsDeposit | CleanupDeposit
PlutusTx.makeIsDataIndexed ''BidDepositRedeemer [('LosingBidder, 0), ('WinningBidder, 1), ('SellerClaimsDeposit, 2), ('CleanupDeposit, 3)]

data FeeEscrowRedeemer = DistributeFees
PlutusTx.makeIsDataIndexed ''FeeEscrowRedeemer [('DistributeFees, 0)]
