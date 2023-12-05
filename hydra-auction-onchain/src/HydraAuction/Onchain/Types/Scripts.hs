module HydraAuction.Onchain.Types.Scripts (
  AuctionMP'Redeemer (..),
  AuctionMP'ScriptHash (..),
  AuctionMetadata'Redeemer (..),
  AuctionMetadata'ScriptHash (..),
) where

import PlutusLedgerApi.V1.Scripts (
  ScriptHash,
 )
import PlutusTx qualified

-- -------------------------------------------------------------------------
-- Auction state token minting policy
-- -------------------------------------------------------------------------
data AuctionMP'Redeemer
  = MintAuction
  | BurnAuction

PlutusTx.unstableMakeIsData ''AuctionMP'Redeemer

newtype AuctionMP'ScriptHash = AuctionMP'ScriptHash
  { sh'AuctionMP :: ScriptHash
  }

-- -------------------------------------------------------------------------
-- Auction metadata validator
-- -------------------------------------------------------------------------
data AuctionMetadata'Redeemer
  = RemoveAuction

PlutusTx.unstableMakeIsData ''AuctionMetadata'Redeemer

newtype AuctionMetadata'ScriptHash = AuctionMetadata'ScriptHash
  { sh'AuctionMetadata :: ScriptHash
  }
