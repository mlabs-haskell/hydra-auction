{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Tx.TermsConfig (
  AuctionTermsConfig (..),
  AuctionTermsDynamic (..),
  nonExistentHeadIdStub,
  constructTermsDynamic,
  configToAuctionTerms,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.TimeMachine (MonadTime)
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import GHC.Generics (Generic)

-- Plutus imports
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Time (POSIXTime (..))
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol (..))
import Plutus.V2.Ledger.Contexts (TxOutRef)
import PlutusTx.Builtins (toBuiltin)

-- Hydra imports
import Hydra.Cardano.Api (TxIn, serialiseToRawBytes, toPlutusTxOutRef)

-- Hydra auction imports
import HydraAuction.OnChain.TestNFT (testNftAssetClass)
import HydraAuction.Types (AuctionTerms (..))
import HydraAuctionUtils.Extras.PlutusOrphans ()
import HydraAuctionUtils.Fixture (Actor, getActorPubKeyHash, keysFor)
import HydraAuctionUtils.Time (currentTimeSeconds)
import HydraAuctionUtils.Types.Natural (Natural)

data AuctionTermsConfig = AuctionTermsConfig
  { configDiffBiddingStart :: !Integer
  , configDiffBiddingEnd :: !Integer
  , configDiffVoucherExpiry :: !Integer
  , configDiffCleanup :: !Integer
  , configAuctionFeePerDelegate :: !Natural
  , configStartingBid :: !Natural
  , configMinimumBidIncrement :: !Natural
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance ToJSON AuctionTermsConfig

instance FromJSON AuctionTermsConfig

data AuctionTermsDynamic = AuctionTermsDynamic
  { configAuctionLot :: !AssetClass
  , -- Storing Actor, not only PubKeyHash,
    -- is required to simplify CLI actions on seller behalf
    configSellerActor :: !Actor
  , configHeadId :: !CurrencySymbol
  , configDelegates :: ![PubKeyHash]
  , configUtxoNonce :: !TxOutRef
  , configAnnouncementTime :: !POSIXTime
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance ToJSON AuctionTermsDynamic

instance FromJSON AuctionTermsDynamic

-- | Stub for tests not checking MoveToHyda. Something not existent.
nonExistentHeadIdStub :: CurrencySymbol
nonExistentHeadIdStub = "DEADBEEF"

constructTermsDynamic ::
  forall (timedMonad :: Type -> Type).
  (MonadTime timedMonad, MonadIO timedMonad) =>
  Actor ->
  TxIn ->
  CurrencySymbol ->
  timedMonad AuctionTermsDynamic
constructTermsDynamic sellerActor utxoNonce headId = do
  currentTimeSeconds' <- currentTimeSeconds
  sellerVkHash <- liftIO $ getActorPubKeyHash sellerActor
  return $
    AuctionTermsDynamic
      { configAuctionLot = testNftAssetClass
      , configSellerActor = sellerActor
      , configHeadId = headId
      , -- FIXME: get actual list of delegates
        configDelegates = [sellerVkHash]
      , configUtxoNonce = toPlutusTxOutRef utxoNonce
      , -- Convert to miliseconds and add one more second to have some time for submiting Tx
        configAnnouncementTime = POSIXTime $ currentTimeSeconds' * 1000 + 1000
      }

configToAuctionTerms ::
  AuctionTermsConfig ->
  AuctionTermsDynamic ->
  IO AuctionTerms
configToAuctionTerms AuctionTermsConfig {..} AuctionTermsDynamic {..} = do
  (sellerVK, _) <- keysFor configSellerActor
  sellerVkHash <- getActorPubKeyHash configSellerActor
  return $
    AuctionTerms
      { auctionLot = configAuctionLot
      , sellerPKH = sellerVkHash
      , sellerVK = toBuiltin $ serialiseToRawBytes sellerVK
      , hydraHeadId = configHeadId
      , delegates = configDelegates
      , biddingStart = toAbsTime configDiffBiddingStart
      , biddingEnd = toAbsTime configDiffBiddingEnd
      , voucherExpiry = toAbsTime configDiffVoucherExpiry
      , cleanup = toAbsTime configDiffCleanup
      , auctionFeePerDelegate = configAuctionFeePerDelegate
      , startingBid = configStartingBid
      , minimumBidIncrement = configMinimumBidIncrement
      , utxoNonce = configUtxoNonce
      }
  where
    (POSIXTime announcementTime) = configAnnouncementTime
    -- Converting seconds to milliseconds
    toAbsTime n = POSIXTime $ announcementTime + (n * 1000)
