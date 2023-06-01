{-

Entities are relation-like data, to be stored and queried in DB-like storage.

Entity class describes their queriable behaviour.

EntityKind is singletone-like functionalization for Entities,
used to uniform queries and storage access.

SomeX datatypes use existencial types to wrap EntityKind usage
and make possible to use single datatype to access different Entities,
as is required for server API.

GADT usage makes deriving not working which affect some design decisions.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HydraAuction.Platform.Interface (
  Entity (..),
  EntityFilter (..),
  AnnouncedAuction (..),
  HydraHead (..),
  HydraHeadInfo (..),
  FilterEq (..),
  FilterOrd (..),
  HeadDelegate (..),
  BidderApproval (..),
  EntityKind (..),
  ServerOutput (..),
  Some (..),
  ClientCommand (..),
  CommandResult (..),
  ClientInput (..),
  EntityQueryResponse (..),
  EntityQuery (..),
) where

-- Prelude imports
import Prelude

-- Haskell imports

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
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
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

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
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

deriving stock instance Entity entity => Eq (EntityQuery entity)
deriving stock instance Entity entity => Generic (EntityQuery entity)
deriving stock instance Entity entity => Show (EntityQuery entity)
deriving anyclass instance Entity entity => FromJSON (EntityQuery entity)
deriving anyclass instance Entity entity => ToJSON (EntityQuery entity)

deriving stock instance Entity entity => Eq (ServerOutput entity)
deriving stock instance Entity entity => Generic (ServerOutput entity)
deriving stock instance Entity entity => Show (ServerOutput entity)
deriving anyclass instance Entity entity => FromJSON (ServerOutput entity)
deriving anyclass instance Entity entity => ToJSON (ServerOutput entity)

-- SomeX instances

{-
EntityKind does not make sense for Command case,
but this type still requires it anyway.
This is designed that way because, GADT/existentials break deriving,
and hiding EntitkyKind existentialy inside EntityQuery will
require to much more manual instances written.
-}

data Some (container :: Type -> Type)
  = forall entity.
    Entity entity =>
    MkSome (EntityKind entity) (container entity)

deriving stock instance
  (forall entity. Entity entity => Show (container entity)) =>
  Show (Some container)

-- | `FromJSON (Some EntityKind)` instance would be Incoherent
parseSomeEntityKindJSON :: Aeson.Value -> Aeson.Parser (Some EntityKind)
parseSomeEntityKindJSON value = case value of
  Aeson.String s ->
    case s of
      "AnnouncedAuction" -> return $ someConstructor AnnouncedAuction
      "HydraHead" -> return $ someConstructor HydraHead
      "HeadDelegate" -> return $ someConstructor HeadDelegate
      "BidderApproval" -> return $ someConstructor BidderApproval
      _ -> fail "Wrong EntityKind tag"
  _ -> fail "Wrong type of kind field"
  where
    -- Some EntityKind contains kind twice :D
    someConstructor kind = MkSome kind kind

-- | Entity container, for which we can serialize `Some container` to JSON
class
  ( forall e. Entity e => FromJSON (container e)
  , forall e. Entity e => ToJSON (container e)
  ) =>
  ContainerForEntity (container :: Type -> Type)
  where
  -- | `Some container` JSON consists of "kind" field for EntityKind tag
  -- | and `jsonSubFieldName` field for `container` content
  -- | Proxy is required for avoiding AmbigiousType
  jsonSubFieldName :: Proxy container -> Aeson.Key

instance
  forall container.
  ContainerForEntity container =>
  (FromJSON (Some container))
  where
  parseJSON value = case value of
    Aeson.Object object -> do
      someKind <-
        Aeson.explicitParseField
          parseSomeEntityKindJSON
          object
          "kind"
      case someKind of
        MkSome kind _ -> do
          container <- object Aeson..: jsonSubFieldName (Proxy @container)
          return $ MkSome @container kind container
    _ -> fail "Wrong type of parsed object"

instance
  forall container.
  ContainerForEntity container =>
  (ToJSON (Some container))
  where
  toJSON (MkSome kind container) =
    Aeson.object
      [ "kind" Aeson..= kind
      , jsonSubFieldName (Proxy @container) Aeson..= container
      ]

instance ContainerForEntity ClientInput where
  jsonSubFieldName Proxy = "input"

instance ContainerForEntity ServerOutput where
  jsonSubFieldName Proxy = "output"
