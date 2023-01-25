module HydraAuction.Addresses (
  VoucherCS (..),
  EscrowAddress (..),
  StandingBidAddress (..),
  FeeEscrowAddress (..),
) where

import PlutusTx.Prelude
import Prelude qualified

import GHC.Generics (Generic)
import Plutus.V1.Ledger.Api (Address)
import Plutus.V1.Ledger.Value (CurrencySymbol)
import PlutusTx qualified

newtype VoucherCS = VoucherCS {unVoucherCS :: CurrencySymbol}
  deriving stock (Generic)
  deriving newtype (Prelude.Eq, Prelude.Show, Eq)

PlutusTx.makeIsDataIndexed ''VoucherCS [('VoucherCS, 0)]
PlutusTx.makeLift ''VoucherCS

newtype EscrowAddress = EscrowAddress {unEscrowAddress :: Address}

PlutusTx.makeLift ''EscrowAddress

newtype StandingBidAddress = StandingBidAddress {unStandingBidAddress :: Address}

PlutusTx.makeLift ''StandingBidAddress

newtype FeeEscrowAddress = FeeEscrowAddress {unFeeEscrowAddress :: Address}

PlutusTx.makeLift ''FeeEscrowAddress
