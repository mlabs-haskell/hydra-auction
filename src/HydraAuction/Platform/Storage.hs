module HydraAuction.Platform.Storage (
  initialStorage,
  queryByPk,
  queryByFilter,
  processCommand,
  processClientInput,
  PlatformImplementation,
) where

-- Prelude imports
import Prelude

-- Haskell imports

import Control.Monad (when)
import Control.Monad.State (MonadState (..), modify')
import Data.Map (Map)
import Data.Map qualified as Map

-- HydraAuction imports

import HydraAuction.Delegate.Interface (InitializedState (..))
import HydraAuction.OnChain (standingBidAddress, voucherCurrencySymbol)
import HydraAuction.Platform.Interface (
  AnnouncedAuction (..),
  BidderApproval (..),
  BidderDeposit (..),
  ClientCommand (..),
  ClientInput (..),
  CommandResult (..),
  Entity (..),
  EntityFilter (..),
  EntityKind (..),
  EntityQuery (..),
  EntityQueryResponse (..),
  FilterEq (..),
  HeadDelegate (..),
  HydraHead (..),
  HydraHeadInfo (..),
  PlatformProtocol,
  ServerOutput (..),
  Some (..),
 )
import HydraAuctionUtils.Server.ClientId (ClientId, ClientResponseScope (..))
import HydraAuctionUtils.Server.Protocol (ProtocolServerLogic (..))
import HydraAuctionUtils.Types.Natural (naturalToInt)

--- Storage

type StorageMap entity = Map (PrimaryKey entity) entity

data EntityStorage = MkEntityStorage
  { storageAnnouncedAuction :: StorageMap AnnouncedAuction
  , storageBidderApproval :: StorageMap BidderApproval
  , storageBidderDeposit :: StorageMap BidderDeposit
  , storageHeadDelegate :: StorageMap HeadDelegate
  , storageHydraHead :: StorageMap HydraHead
  }
  deriving stock (Eq, Show)

initialStorage :: EntityStorage
initialStorage =
  MkEntityStorage
    { storageAnnouncedAuction = Map.empty
    , storageBidderApproval = Map.empty
    , storageBidderDeposit = Map.empty
    , storageHeadDelegate = Map.empty
    , storageHydraHead = Map.empty
    }

-- FIXME: yes, this should have been a lens
entityStorageMap :: EntityKind entity -> EntityStorage -> StorageMap entity
entityStorageMap entity = selector
  where
    selector = case entity of
      AnnouncedAuction -> storageAnnouncedAuction
      BidderApproval -> storageBidderApproval
      BidderDeposit -> storageBidderDeposit
      HeadDelegate -> storageHeadDelegate
      HydraHead -> storageHydraHead

overStorageMap ::
  EntityKind entity ->
  (StorageMap entity -> StorageMap entity) ->
  (EntityStorage -> EntityStorage)
overStorageMap entity func storage = case entity of
  AnnouncedAuction ->
    storage
      { storageAnnouncedAuction =
          func $ storageAnnouncedAuction storage
      }
  BidderApproval ->
    storage
      { storageBidderApproval =
          func $ storageBidderApproval storage
      }
  BidderDeposit ->
    storage
      { storageBidderDeposit =
          func $ storageBidderDeposit storage
      }
  HeadDelegate ->
    storage
      { storageHeadDelegate =
          func $ storageHeadDelegate storage
      }
  HydraHead ->
    storage
      { storageHydraHead =
          func $ storageHydraHead storage
      }

-- Storage query/insert

queryByPk ::
  (Entity entity) =>
  EntityKind entity ->
  PrimaryKey entity ->
  EntityStorage ->
  Maybe entity
queryByPk kind primaryKey storage =
  Map.lookup primaryKey (entityStorageMap kind storage)

queryByFilter ::
  (MonadState EntityStorage m, Entity entity) =>
  EntityKind entity ->
  EntityQuery entity ->
  m (EntityQueryResponse entity)
queryByFilter kind (MkQuery filters mLimit) = do
  storage <- get
  return $
    MkResponse $
      takeToMLimit $
        filter filterPred $
          Map.elems (entityStorageMap kind storage)
  where
    filterPred entity = all (isMatchingEntityFilter entity) filters
    takeToMLimit = maybe id take mLimit

insert ::
  (MonadState EntityStorage m, Entity entity) =>
  EntityKind entity ->
  entity ->
  m ()
insert kind entity = do
  modify' $
    overStorageMap kind $
      Map.insert (getPrimaryKey entity) entity

insertIfNotExisting ::
  (MonadState EntityStorage m, Entity entity) =>
  EntityKind entity ->
  entity ->
  m Bool
insertIfNotExisting kind entity = do
  storage <- get
  case queryByPk kind (getPrimaryKey entity) storage of
    Just _ -> return False
    Nothing -> do
      insert kind entity
      return True

processCommand ::
  MonadState EntityStorage m => ClientCommand -> m CommandResult
processCommand command = case command of
  ReportAnnouncedAuction terms ->
    reportInserted =<< insertIfNotExisting AnnouncedAuction newEntity
    where
      newEntity =
        MkAnnouncedAuction
          { auctionTerms = terms
          , auctionId = voucherCurrencySymbol terms
          , auctionStandingBidAddress = standingBidAddress terms
          }
  ReportBidderApproval approval ->
    reportInserted =<< insertIfNotExisting BidderApproval approval
  ReportBidderDeposit deposit ->
    reportInserted =<< insertIfNotExisting BidderDeposit deposit
  ReportHeadDelegate staticInfo delegateActor -> do
    -- Head could be same in the reports by different delegates
    -- First write wins
    _ <- insertIfNotExisting HydraHead hydraHead
    inserted <- insertIfNotExisting HeadDelegate headDelegate
    when inserted updateIfAllDelegatesKnown
    reportInserted inserted
    where
      hydraHead =
        MkHydraHead
          { staticInfo
          , allDelegatesKnown = False
          , headDelegateState =
              -- FIXME
              AwaitingCommits {stangingBidWasCommited = False}
          }
      headDelegate = MkHeadDelegate (getPrimaryKey hydraHead) delegateActor
      updateIfAllDelegatesKnown = do
        MkResponse delegatesReported <-
          queryByFilter
            HeadDelegate
            ( MkQuery [DelegateByHeadId $ Eq $ getPrimaryKey hydraHead] Nothing
            )
        when
          (length delegatesReported == expectedDelegatesNum)
          (insert HydraHead $ hydraHead {allDelegatesKnown = True})
      expectedDelegatesNum =
        fromIntegral $
          naturalToInt $
            delegatesNumber staticInfo
  where
    reportInserted inserted = do
      if inserted
        then return EntityCreated
        else return EntityAlreadyExists

processClientInput' ::
  forall entity m.
  (Entity entity, MonadState EntityStorage m) =>
  EntityKind entity ->
  ClientInput entity ->
  m (ServerOutput entity)
processClientInput' kind input = case input of
  Query query -> QueryPerformed <$> queryByFilter kind query
  Command command -> CommandResult <$> processCommand command

processClientInput ::
  MonadState EntityStorage m => Some ClientInput -> m (Some ServerOutput)
processClientInput (MkSome kind input) = do
  x <- processClientInput' kind input
  return $ MkSome kind x

data PlatformImplementation

instance ProtocolServerLogic PlatformImplementation where
  type ImplementationFor PlatformImplementation = PlatformProtocol
  type State PlatformImplementation = EntityStorage
  initialState = initialStorage
  implementation ::
    (Monad m, MonadState EntityStorage m) =>
    (ClientId, Some ClientInput) ->
    m [(ClientResponseScope, Some ServerOutput)]
  implementation (clientId, input) = do
    output <- processClientInput input
    return [(PerClient clientId, output)]
