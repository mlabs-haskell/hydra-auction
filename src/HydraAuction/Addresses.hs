module HydraAuction.Addresses (
  VoucherCS (..),
  EscrowAddress (..),
  StandingBidAddress (..),
  FeeEscrowAddress (..),
  DepositAddress (..),
) where

-- Prelude imports
import PlutusTx.Prelude
import Prelude qualified

-- Haskell imports

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

-- Plutus imports

import PlutusLedgerApi.V1.Address (Address)
import PlutusLedgerApi.V1.Value (CurrencySymbol)
import PlutusTx qualified

-- HydraAuction imports
import HydraAuctionUtils.Extras.PlutusOrphans ()

newtype VoucherCS = VoucherCS {unVoucherCS :: CurrencySymbol}
  deriving stock (Generic)
  deriving newtype (Prelude.Eq, Prelude.Ord, Prelude.Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''VoucherCS [('VoucherCS, 0)]
PlutusTx.makeLift ''VoucherCS

newtype EscrowAddress = EscrowAddress {unEscrowAddress :: Address}

PlutusTx.makeLift ''EscrowAddress

newtype StandingBidAddress = StandingBidAddress {unStandingBidAddress :: Address}
  deriving newtype (Prelude.Eq, Prelude.Ord, Prelude.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''StandingBidAddress

newtype FeeEscrowAddress = FeeEscrowAddress {unFeeEscrowAddress :: Address}

PlutusTx.makeLift ''FeeEscrowAddress

newtype DepositAddress = DepositAddress {unDepositAddress :: Address}

PlutusTx.makeLift ''DepositAddress
