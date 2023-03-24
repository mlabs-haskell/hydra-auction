{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Tx.TermsConfig (
  AuctionTermsConfig (..),
  AuctionTermsDynamic (..),
  constructTermsDynamic,
  configToAuctionTerms,
) where

-- Prelude imports
import PlutusTx.Prelude (emptyByteString)
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

-- Hydra imports
import Hydra.Cardano.Api (TxIn, toPlutusKeyHash, toPlutusTxOutRef, verificationKeyHash)

-- Hydra auction imports
import HydraAuction.OnChain.TestNFT (testNftAssetClass)
import HydraAuction.Plutus.Orphans ()
import HydraAuction.Tx.Common (currentTimeSeconds)
import HydraAuction.Types (AuctionTerms (..), Natural)
import HydraAuctionUtils.Fixture (Actor, keysFor)

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
  , -- Storing Actor, not only PubKeyHash, is required to simplify CLI actions on seller behalf
    configSellerActor :: !Actor
  , configDelegates :: ![PubKeyHash]
  , configUtxoNonce :: !TxOutRef
  , configAnnouncementTime :: !POSIXTime
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance ToJSON AuctionTermsDynamic

instance FromJSON AuctionTermsDynamic

getActorVkHash :: Actor -> IO PubKeyHash
getActorVkHash actor = do
  (actorVk, _) <- keysFor actor
  return $ toPlutusKeyHash $ verificationKeyHash actorVk

constructTermsDynamic ::
  forall (timedMonad :: Type -> Type). (MonadTime timedMonad, MonadIO timedMonad) => Actor -> TxIn -> timedMonad AuctionTermsDynamic
constructTermsDynamic sellerActor utxoNonce = do
  currentTimeSeconds' <- currentTimeSeconds
  sellerVkHash <- liftIO $ getActorVkHash sellerActor
  return $
    AuctionTermsDynamic
      { configAuctionLot = testNftAssetClass
      , configSellerActor = sellerActor
      , -- FIXME
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
  sellerVkHash <- getActorVkHash configSellerActor
  return $
    AuctionTerms
      { auctionLot = configAuctionLot
      , seller = sellerVkHash
      , -- FUTURE FIXME
        hydraHeadId = CurrencySymbol emptyByteString
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
