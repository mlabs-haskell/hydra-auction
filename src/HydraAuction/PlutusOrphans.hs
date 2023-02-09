{-# OPTIONS -Wno-orphans #-}

-- | Orphan instances for Plutus, Hydra and Cardano API
module HydraAuction.PlutusOrphans () where

import Prelude

import Control.Monad (when)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteString.Base16 qualified as Base16
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Hydra.Cluster.Fixture (Actor (..))
import Plutus.V1.Ledger.Crypto (PubKeyHash (..))
import Plutus.V1.Ledger.Time (POSIXTime (..))
import Plutus.V1.Ledger.Value (AssetClass (..), CurrencySymbol (..), TokenName (..))
import Plutus.V2.Ledger.Contexts (TxOutRef)
import Plutus.V2.Ledger.Tx (TxId (..))
import PlutusTx.Builtins.Internal (BuiltinByteString (..))

-- Time

instance FromJSON POSIXTime where
  parseJSON x = do
    int <- parseJSON x
    when (int < 0) $
      fail "Negative number given for POSIXTime"
    return $ POSIXTime int

deriving via Integer instance (ToJSON POSIXTime)

-- Basic types

instance FromJSON BuiltinByteString where
  parseJSON x = BuiltinByteString . either (error errorMessage) id . Base16.decode . encodeUtf8 <$> parseJSON x
    where
      errorMessage = "Cannot decode base 16 in JSON field"

instance ToJSON BuiltinByteString where
  toJSON (BuiltinByteString bs) = toJSON $ decodeUtf8 $ Base16.encode bs

-- Asset class

-- FIXME: serialize using fields
deriving newtype instance (FromJSON AssetClass)
deriving newtype instance (ToJSON AssetClass)

deriving via BuiltinByteString instance (FromJSON CurrencySymbol)
deriving via BuiltinByteString instance (ToJSON CurrencySymbol)
deriving via BuiltinByteString instance (FromJSON TokenName)
deriving via BuiltinByteString instance (ToJSON TokenName)

-- Transactions

deriving via BuiltinByteString instance (FromJSON PubKeyHash)
deriving via BuiltinByteString instance (ToJSON PubKeyHash)

instance (FromJSON TxOutRef)
instance (ToJSON TxOutRef)

deriving via BuiltinByteString instance (FromJSON TxId)
deriving via BuiltinByteString instance (ToJSON TxId)

-- Hydra

deriving stock instance (Generic Actor)

instance FromJSON Actor
instance ToJSON Actor
