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
import GHC.Generics (Generic)

-- Plutus imports
import Plutus.V1.Ledger.Value (CurrencySymbol)
import Plutus.V2.Ledger.Api (Address)
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

newtype DepositAddress = DepositAddress {unDepositAddress :: Address}

PlutusTx.makeLift ''DepositAddress
