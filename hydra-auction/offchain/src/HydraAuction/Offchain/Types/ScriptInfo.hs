module HydraAuction.Offchain.Types.ScriptInfo (
  AuctionScriptInfo (..),
  auctionScriptsToInfo,
) where

import Prelude

import GeniusYield.Types (
  GYMintingPolicy,
  GYNetworkId,
  GYValidator,
  PlutusVersion (..),
  addressFromValidator,
  addressToPlutus,
  mintingPolicyCurrencySymbol,
 )

import HydraAuction.Onchain.Types.AuctionInfo (AuctionInfo (..))
import HydraAuction.Onchain.Types.AuctionTerms (AuctionTerms)

-- -------------------------------------------------------------------------
-- Auction script info
-- -------------------------------------------------------------------------

data AuctionScriptInfo = AuctionScriptInfo
  { as'AuctionTerms :: AuctionTerms
  , as'AuctionMp :: GYMintingPolicy 'PlutusV2
  , as'AuctionEscrow :: GYValidator 'PlutusV2
  , as'BidderDeposit :: GYValidator 'PlutusV2
  , as'FeeEscrow :: GYValidator 'PlutusV2
  , as'StandingBid :: GYValidator 'PlutusV2
  }

auctionScriptsToInfo :: GYNetworkId -> AuctionScriptInfo -> AuctionInfo
auctionScriptsToInfo nw AuctionScriptInfo {..} =
  AuctionInfo
    { ai'AuctionCs = mintingPolicyCurrencySymbol as'AuctionMp
    , ai'AuctionTerms = as'AuctionTerms
    , ai'AuctionEscrow = asPlutusAddress as'AuctionEscrow
    , ai'BidderDeposit = asPlutusAddress as'BidderDeposit
    , ai'FeeEscrow = asPlutusAddress as'FeeEscrow
    , ai'StandingBid = asPlutusAddress as'StandingBid
    }
  where
    asPlutusAddress = addressToPlutus . addressFromValidator nw
