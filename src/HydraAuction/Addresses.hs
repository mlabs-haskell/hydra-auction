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

data VoucherCS = VoucherCS {unVoucherCS :: CurrencySymbol}
  deriving stock (Generic)
  deriving stock (Prelude.Eq, Prelude.Show)

instance Eq VoucherCS where
  {-# INLINEABLE (==) #-}
  (VoucherCS x') == (VoucherCS y') = (x' == y')

PlutusTx.makeIsDataIndexed ''VoucherCS [('VoucherCS, 0)]
PlutusTx.makeLift ''VoucherCS

data EscrowAddress = EscrowAddress {unEscrowAddress :: Address}

PlutusTx.makeIsDataIndexed ''EscrowAddress [('EscrowAddress, 0)]
PlutusTx.makeLift ''EscrowAddress

data StandingBidAddress = StandingBidAddress {unStandingBidAddress :: Address}

PlutusTx.makeIsDataIndexed ''StandingBidAddress [('StandingBidAddress, 0)]
PlutusTx.makeLift ''StandingBidAddress

data FeeEscrowAddress = FeeEscrowAddress {unFeeEscrowAddress :: Address}

PlutusTx.makeIsDataIndexed ''FeeEscrowAddress [('FeeEscrowAddress, 0)]
PlutusTx.makeLift ''FeeEscrowAddress
