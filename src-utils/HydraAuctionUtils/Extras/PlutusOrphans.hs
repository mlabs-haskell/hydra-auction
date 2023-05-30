{-# OPTIONS -Wno-orphans #-}

-- | Orphan instances for Plutus, Hydra and Cardano API
module HydraAuctionUtils.Extras.PlutusOrphans () where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad (when)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteString.Base16 qualified as Base16
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- Plutus imports
import PlutusLedgerApi.V1.Credential (Credential, StakingCredential)
import PlutusLedgerApi.V1.Crypto (PubKeyHash (..))
import PlutusLedgerApi.V1.Scripts (ScriptHash (..))
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusLedgerApi.V1.Value (AssetClass (..), CurrencySymbol (..), TokenName (..))
import PlutusLedgerApi.V2.Contexts (TxOutRef)
import PlutusLedgerApi.V2.Tx (TxId (..))
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

-- Addresses

deriving newtype instance (ToJSON ScriptHash)
deriving newtype instance (FromJSON ScriptHash)

deriving anyclass instance (ToJSON Credential)
deriving anyclass instance (FromJSON Credential)
deriving anyclass instance (ToJSON StakingCredential)
deriving anyclass instance (FromJSON StakingCredential)

-- Asset class

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
