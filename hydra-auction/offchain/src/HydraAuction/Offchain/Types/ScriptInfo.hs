module HydraAuction.Offchain.Types.ScriptInfo (
  AuctionScriptInfo (..),
  auctionScriptsToInfo,
) where

import Prelude

import GeniusYield.Types (
  GYNetworkId,
  addressFromValidator,
  addressToPlutus,
  mintingPolicyCurrencySymbol,
 )

import HydraAuction.Onchain.Types.AuctionInfo qualified as O
import HydraAuction.Onchain.Types.AuctionTerms qualified as O

import HydraAuction.Offchain.Types.Scripts (
  AuctionEscrowV (..),
  BidderDepositV (..),
  FeeEscrowV (..),
  StandingBidV (..),
 )
import HydraAuction.Offchain.Types.Tokens (AuctionMp (..))

-- -------------------------------------------------------------------------
-- Auction script info
-- -------------------------------------------------------------------------

data AuctionScriptInfo = AuctionScriptInfo
  { si'AuctionMp :: AuctionMp
  , si'AuctionTerms :: O.AuctionTerms
  , si'AuctionEscrow :: AuctionEscrowV
  , si'BidderDeposit :: BidderDepositV
  , si'FeeEscrow :: FeeEscrowV
  , si'StandingBid :: StandingBidV
  }

auctionScriptsToInfo :: GYNetworkId -> AuctionScriptInfo -> O.AuctionInfo
auctionScriptsToInfo nw AuctionScriptInfo {..} =
  O.AuctionInfo
    { ai'AuctionCs = mintingPolicyCurrencySymbol auctionMp
    , ai'AuctionTerms = si'AuctionTerms
    , ai'AuctionEscrow = asPlutusAddress auctionEscrowV
    , ai'BidderDeposit = asPlutusAddress bidderDepositV
    , ai'FeeEscrow = asPlutusAddress feeEscrowV
    , ai'StandingBid = asPlutusAddress standingBidV
    }
  where
    AuctionMp {..} = si'AuctionMp
    AuctionEscrowV {..} = si'AuctionEscrow
    BidderDepositV {..} = si'BidderDeposit
    FeeEscrowV {..} = si'FeeEscrow
    StandingBidV {..} = si'StandingBid
    asPlutusAddress = addressToPlutus . addressFromValidator nw
