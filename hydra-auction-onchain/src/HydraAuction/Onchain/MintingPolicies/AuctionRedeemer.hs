module HydraAuction.Onchain.MintingPolicies.AuctionRedeemer (
  AuctionMP'Redeemer (..),
) where

import PlutusTx qualified

data AuctionMP'Redeemer
  = MintAuction
  | BurnAuction

PlutusTx.unstableMakeIsData ''AuctionMP'Redeemer
