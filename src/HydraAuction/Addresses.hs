module HydraAuction.Addresses (
  VoucherCS (..),
  EscrowAddress (..),
  StandingBidAddress (..),
  FeeEscrowAddress (..),
) where

import Plutus.V1.Ledger.Api (Address)
import Plutus.V1.Ledger.Value (CurrencySymbol)
import PlutusTx qualified

newtype VoucherCS = VoucherCS {unVoucherCS :: CurrencySymbol}

newtype EscrowAddress = EscrowAddress {unEscrowAddress :: Address}

PlutusTx.makeLift ''EscrowAddress

newtype StandingBidAddress = StandingBidAddress {unStandingBidAddress :: Address}

PlutusTx.makeLift ''StandingBidAddress

newtype FeeEscrowAddress = FeeEscrowAddress {unFeeEscrowAddress :: Address}

PlutusTx.makeLift ''FeeEscrowAddress
