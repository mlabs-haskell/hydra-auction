module HydraAuction.Addresses (
  unStandingBidAddress,
  unEscrowAddress,
  VoucherCS (..),
  EscrowAddress (..),
  StandingBidAddress (..),
) where

import Plutus.V1.Ledger.Api (Address)
import Plutus.V1.Ledger.Value (CurrencySymbol)
import PlutusTx qualified

newtype VoucherCS = VoucherCS CurrencySymbol

newtype EscrowAddress = EscrowAddress Address

PlutusTx.makeLift ''EscrowAddress

unEscrowAddress :: EscrowAddress -> Address
unEscrowAddress (EscrowAddress address) = address

newtype StandingBidAddress = StandingBidAddress Address

PlutusTx.makeLift ''StandingBidAddress

unStandingBidAddress :: StandingBidAddress -> Address
unStandingBidAddress (StandingBidAddress address) = address
