module HydraAuction.Offchain.Types.Scripts (
  AuctionEscrowV (..),
  AuctionMetadataV (..),
  BidderDepositV (..),
  FeeEscrowV (..),
  StandingBidV (..),
  --
  auctionEscrowVToOnchain,
  auctionMetadataVToOnchain,
  bidderDepositVToOnchain,
  feeEscrowVToOnchain,
  standingBidVToOnchain,
) where

import Prelude

import GeniusYield.Types (
  GYValidator,
  PlutusVersion (..),
  validatorHash,
  validatorHashToPlutus,
 )

import HydraAuction.Onchain.Types.Scripts qualified as O

type GYValidatorV2 = GYValidator 'PlutusV2

-- -------------------------------------------------------------------------
-- Auction escrow validator
-- -------------------------------------------------------------------------
newtype AuctionEscrowV = AuctionEscrowV
  { auctionEscrowV :: GYValidatorV2
  }

auctionEscrowVToOnchain :: AuctionEscrowV -> O.AuctionEscrow'ScriptHash
auctionEscrowVToOnchain AuctionEscrowV {..} =
  O.AuctionEscrow'ScriptHash $
    validatorHashToPlutus $
      validatorHash auctionEscrowV

-- -------------------------------------------------------------------------
-- Auction metadata validator
-- -------------------------------------------------------------------------
newtype AuctionMetadataV = AuctionMetadataV
  { auctionMetadataV :: GYValidatorV2
  }

auctionMetadataVToOnchain :: AuctionMetadataV -> O.AuctionMetadata'ScriptHash
auctionMetadataVToOnchain AuctionMetadataV {..} =
  O.AuctionMetadata'ScriptHash $
    validatorHashToPlutus $
      validatorHash auctionMetadataV

-- -------------------------------------------------------------------------
-- Bidder deposit validator
-- -------------------------------------------------------------------------
newtype BidderDepositV = BidderDepositV
  { bidderDepositV :: GYValidatorV2
  }

bidderDepositVToOnchain :: BidderDepositV -> O.BidderDeposit'ScriptHash
bidderDepositVToOnchain BidderDepositV {..} =
  O.BidderDeposit'ScriptHash $
    validatorHashToPlutus $
      validatorHash bidderDepositV

-- -------------------------------------------------------------------------
-- Fee escrow validator
-- -------------------------------------------------------------------------
newtype FeeEscrowV = FeeEscrowV
  { feeEscrowV :: GYValidatorV2
  }

feeEscrowVToOnchain :: FeeEscrowV -> O.FeeEscrow'ScriptHash
feeEscrowVToOnchain FeeEscrowV {..} =
  O.FeeEscrow'ScriptHash $
    validatorHashToPlutus $
      validatorHash feeEscrowV

-------------------------------------------------------------------------
-- Standing bid validator
-- -------------------------------------------------------------------------
newtype StandingBidV = StandingBidV
  { standingBidV :: GYValidatorV2
  }

standingBidVToOnchain :: StandingBidV -> O.StandingBid'ScriptHash
standingBidVToOnchain StandingBidV {..} =
  O.StandingBid'ScriptHash $
    validatorHashToPlutus $
      validatorHash standingBidV
