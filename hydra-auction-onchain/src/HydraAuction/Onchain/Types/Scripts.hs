module HydraAuction.Onchain.Types.Scripts (
  AuctionMP'Redeemer (..),
  AuctionID (..),
  AuctionMP'ScriptHash (..),
  --
  AuctionMetadata'Redeemer (..),
  AuctionMetadata'ScriptHash (..),
  --
  FeeEscrow'Redeemer (..),
  FeeEscrow'ScriptHash (..),
  --
  StandingBid'Redeemer (..),
  StandingBid'ScriptHash (..),
) where

import PlutusLedgerApi.V1.Scripts (
  ScriptHash,
 )
import PlutusLedgerApi.V1.Value (
  CurrencySymbol,
 )
import PlutusTx qualified

-- -------------------------------------------------------------------------
-- Auction state token minting policy
-- -------------------------------------------------------------------------
data AuctionMP'Redeemer
  = MintAuction
  | BurnAuction

PlutusTx.unstableMakeIsData ''AuctionMP'Redeemer

newtype AuctionID = AuctionID
  { auctionID :: CurrencySymbol
  }

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

-- -------------------------------------------------------------------------
-- Fee escrow validator
-- -------------------------------------------------------------------------
data FeeEscrow'Redeemer
  = DistributeFees

PlutusTx.unstableMakeIsData ''FeeEscrow'Redeemer

newtype FeeEscrow'ScriptHash = FeeEscrow'ScriptHash
  { sh'FeeEscrow :: ScriptHash
  }

-- -------------------------------------------------------------------------
-- Standing bid validator
-- -------------------------------------------------------------------------
data StandingBid'Redeemer
  = NewBid
  | MoveToHydra
  | ConcludeAuction

PlutusTx.unstableMakeIsData ''StandingBid'Redeemer

newtype StandingBid'ScriptHash = StandingBid'ScriptHash
  { sh'StandingBid :: ScriptHash
  }
