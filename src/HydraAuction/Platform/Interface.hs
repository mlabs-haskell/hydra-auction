{-# OPTIONS_GHC -Wno-orphans #-}

module HydraAuction.Platform.Interface (
  Entity (..),
  AnnouncedAuction (..),
  HydraHead (..),
  HydraHeadInfo (..),
  HeadDelegate (..),
  BidderApproval (..),
  EntityKind (..),
  ServerOutput (..),
  SomeServerOutput (..),
  ClientCommand (..),
  CommandResult (..),
  ClientInput (..),
  SomeClientInput (..),
  EntityQueryResponse (..),
  EntityQuery (..),
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text qualified as T
import GHC.Generics (Generic)

-- Plutus imports
import PlutusLedgerApi.V2 (BuiltinByteString)

-- Hydra imports

import Hydra.Cardano.Api (Lovelace)
import Hydra.Chain (HeadId (..))

-- HydraAuction imports

import HydraAuction.Addresses (StandingBidAddress, VoucherCS)
import HydraAuction.Delegate.Interface (
  InitializedState,
  InitializedStateKind,
  initializedStateKind,
 )
import HydraAuction.Types (AuctionTerms)
import HydraAuctionUtils.Fixture (Actor)
import HydraAuctionUtils.Types.Natural (Natural)

-- Interfaces

type DefaultConstraints x = (Eq x, Show x, ToJSON x, FromJSON x, Generic x)

class
  ( Ord (PrimaryKey entity)
  , DefaultConstraints (EntityFilter entity)
  , DefaultConstraints (PrimaryKey entity)
  , DefaultConstraints entity
  ) =>
  Entity entity
  where
  type PrimaryKey entity
  data EntityFilter entity

  getPrimaryKey :: entity -> PrimaryKey entity
  isMatchingEntityFilter :: entity -> EntityFilter entity -> Bool

-- Entities

data AnnouncedAuction = MkAnnouncedAuction
  { terms :: AuctionTerms
  , auctionId :: VoucherCS
  , -- Required for Delegate Servers
    auctionStandingBidAddress :: StandingBidAddress
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (ToJSON, FromJSON)

data HydraHeadInfo = MkHydraHeadInfo
  { headId :: HeadId
  , delegatesNumber :: Natural
  , auctionFeePerDelegate :: Lovelace
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON)

data HydraHead = MkHydraHead
  { staticInfo :: HydraHeadInfo
  , allDelegatesKnown :: Bool
  , headDelegateState :: InitializedState
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

deriving stock instance Ord HeadId

data HeadDelegate = MkHeadDelegate
  { delegateHeadId :: HeadId
  , -- IRL this should be PKH for
    delegateActor :: Actor
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON)

data BidderApproval = MkBidderApproval
  { approvedAuctionId :: VoucherCS
  , bidder :: Actor
  , approvalBytes :: BuiltinByteString
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON)

-- Generic filter classes/datatype

data FilterEq x = Eq x | IsIn [x]
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data FilterOrd x = Lte x | Lt x | Gte x | Gt x
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

class ValueFilter filter v where
  isMatchingFilter :: v -> filter -> Bool

instance (Eq value) => ValueFilter (FilterEq value) value where
  isMatchingFilter y (Eq x) = x == y
  isMatchingFilter y (IsIn xs) = y `elem` xs

instance (Ord value) => ValueFilter (FilterOrd value) value where
  isMatchingFilter value condition = case condition of
    Lte x -> value <= x
    Lt x -> value < x
    Gte x -> value >= x
    Gt x -> value > x

--- Entity instances

instance Entity AnnouncedAuction where
  type PrimaryKey AnnouncedAuction = VoucherCS
  data EntityFilter AnnouncedAuction
    = ByAuctionId (FilterEq VoucherCS)
    | ByStandingBidAddress (FilterEq StandingBidAddress)
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

  getPrimaryKey = auctionId
  isMatchingEntityFilter entity filter' =
    case filter' of
      ByAuctionId x ->
        isMatchingFilter (auctionId entity) x
      ByStandingBidAddress x ->
        isMatchingFilter (auctionStandingBidAddress entity) x

instance Entity HydraHead where
  type PrimaryKey HydraHead = HeadId
  data EntityFilter HydraHead
    = ByHeadId (FilterEq HeadId)
    | ByDelegatesNumber (FilterOrd Natural)
    | ByFeePerDelegate (FilterOrd Lovelace)
    | ByAllDelegatesKnown (FilterEq Bool)
    | ByDelegateStateKind (FilterOrd InitializedStateKind)
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

  getPrimaryKey entity = headId $ staticInfo entity
  isMatchingEntityFilter entity filter' =
    case filter' of
      ByHeadId x ->
        isMatchingFilter (getPrimaryKey entity) x
      ByDelegatesNumber x ->
        isMatchingFilter (delegatesNumber $ staticInfo entity) x
      ByFeePerDelegate x ->
        isMatchingFilter
          (auctionFeePerDelegate $ staticInfo entity)
          x
      ByAllDelegatesKnown x ->
        isMatchingFilter (allDelegatesKnown entity) x
      ByDelegateStateKind x ->
        isMatchingFilter
          (initializedStateKind $ headDelegateState entity)
          x

instance Entity HeadDelegate where
  type PrimaryKey HeadDelegate = HeadDelegate
  data EntityFilter HeadDelegate
    = DelegateByHeadId (FilterEq HeadId)
    | ByDelegateActor (FilterEq Actor)
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

  getPrimaryKey = id
  isMatchingEntityFilter entity filter' =
    case filter' of
      DelegateByHeadId x ->
        isMatchingFilter (delegateHeadId entity) x
      ByDelegateActor x ->
        isMatchingFilter (delegateActor entity) x

instance Entity BidderApproval where
  type PrimaryKey BidderApproval = (VoucherCS, Actor)
  data EntityFilter BidderApproval
    = ByApprovedAuctionId (FilterEq VoucherCS)
    | ByApprovedBidder (FilterEq Actor)
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

  getPrimaryKey entity = (approvedAuctionId entity, bidder entity)
  isMatchingEntityFilter entity filter' =
    case filter' of
      ByApprovedAuctionId x ->
        isMatchingFilter (approvedAuctionId entity) x
      ByApprovedBidder x ->
        isMatchingFilter (bidder entity) x

-- EntityKind

data EntityKind entity where
  AnnouncedAuction :: EntityKind AnnouncedAuction
  HydraHead :: EntityKind HydraHead
  HeadDelegate :: EntityKind HeadDelegate
  BidderApproval :: EntityKind BidderApproval

instance Show (EntityKind x) where
  show x = case x of
    AnnouncedAuction -> "AnnouncedAuction"
    HydraHead -> "HydraHead"
    HeadDelegate -> "HeadDelegate"
    BidderApproval -> "BidderApproval"

instance ToJSON (EntityKind x) where
  toJSON = Aeson.String . T.pack . show

-- Input/Output

data ClientInput entity
  = Query (EntityQuery entity)
  | Command ClientCommand

data ClientCommand
  = ReportAnnouncedAuction AuctionTerms
  | ReportBidderApproval BidderApproval
  | ReportHeadDelegate HydraHeadInfo Actor
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON)

data ServerOutput entity
  = CommandResult CommandResult
  | QueryPerformed (EntityQueryResponse entity)

data CommandResult = EntityCreated | EntityAlreadyExists
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON)

data EntityQuery entity = MkQuery
  { filters :: [EntityFilter entity]
  , limit :: Maybe Int
  }
newtype EntityQueryResponse entity = MkResponse [entity]

deriving stock instance Entity entity => Eq (EntityQuery entity)
deriving stock instance Entity entity => Generic (EntityQuery entity)
deriving stock instance Entity entity => Show (EntityQuery entity)
deriving anyclass instance Entity entity => FromJSON (EntityQuery entity)
deriving anyclass instance Entity entity => ToJSON (EntityQuery entity)

deriving stock instance Entity entity => Eq (ClientInput entity)
deriving stock instance Entity entity => Generic (ClientInput entity)
deriving stock instance Entity entity => Show (ClientInput entity)
deriving anyclass instance Entity entity => FromJSON (ClientInput entity)
deriving anyclass instance Entity entity => ToJSON (ClientInput entity)

deriving stock instance Entity entity => Eq (ServerOutput entity)
deriving stock instance Entity entity => Generic (ServerOutput entity)
deriving stock instance Entity entity => Show (ServerOutput entity)
deriving anyclass instance Entity entity => FromJSON (ServerOutput entity)
deriving anyclass instance Entity entity => ToJSON (ServerOutput entity)

deriving stock instance Entity entity => Eq (EntityQueryResponse entity)
deriving stock instance Entity entity => Generic (EntityQueryResponse entity)
deriving stock instance Entity entity => Show (EntityQueryResponse entity)
deriving anyclass instance Entity entity => FromJSON (EntityQueryResponse entity)
deriving anyclass instance Entity entity => ToJSON (EntityQueryResponse entity)

-- SomeX instances

data SomeClientInput where
  MkSomeClientInput ::
    forall entity.
    Entity entity =>
    EntityKind entity ->
    ClientInput entity ->
    SomeClientInput

fromKindedObjectJSON ::
  forall some f.
  (forall e. Entity e => Aeson.Object -> Aeson.Parser (f e)) ->
  (forall e. Entity e => EntityKind e -> f e -> some) ->
  Aeson.Value ->
  Aeson.Parser some
fromKindedObjectJSON dependentParser someConstr value = case value of
  (Aeson.Object object) ->
    case Aeson.lookup "kind" object of
      Just (Aeson.String s) ->
        case s of
          -- This cannot be refactored into local function,
          -- due to GADT type checking
          "AnnouncedAuction" ->
            someConstr AnnouncedAuction <$> dependentParser object
          "HydraHead" ->
            someConstr HydraHead <$> dependentParser object
          "HeadDelegate" ->
            someConstr HeadDelegate <$> dependentParser object
          "BidderApproval" ->
            someConstr BidderApproval <$> dependentParser object
          _ -> fail "Wrong EntityKind tag"
      _ -> fail "Wrong type of kind field"
  _ -> fail "Wrong type of parsed object"

instance FromJSON SomeClientInput where
  parseJSON = fromKindedObjectJSON (Aeson..: "input") MkSomeClientInput

instance Show SomeClientInput where
  show (MkSomeClientInput kind input) =
    "MkSomeClientInput " <> show kind <> " " <> show input

instance ToJSON SomeClientInput where
  toJSON (MkSomeClientInput kind input) =
    Aeson.object
      [ "kind" Aeson..= kind
      , "input" Aeson..= input
      ]

data SomeServerOutput where
  MkSomeServerOutput ::
    forall entity.
    Entity entity =>
    EntityKind entity ->
    ServerOutput entity ->
    SomeServerOutput

instance Show SomeServerOutput where
  show (MkSomeServerOutput kind output) =
    "MkSomeServerOutput " <> show kind <> " " <> show output

instance FromJSON SomeServerOutput where
  parseJSON =
    fromKindedObjectJSON (Aeson..: "output") MkSomeServerOutput

instance ToJSON SomeServerOutput where
  toJSON (MkSomeServerOutput kind output) =
    Aeson.object
      [ "kind" Aeson..= kind
      , "output" Aeson..= output
      ]
