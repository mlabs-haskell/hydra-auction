module HydraAuction.Offchain.Scripts (
  mkAuctionScriptInfo,
  --
  auctionMetadataS,
  mkAuctionEscrowS,
  mkAuctionMpS,
  mkFeeEscrowS,
  mkStandingBidS,
  mkBidderDepositS,
) where

import Prelude

import GeniusYield.Types (
  GYMintingPolicy,
  GYTxOutRef,
  GYValidator,
  PlutusVersion (..),
  mintingPolicyFromPlutus,
  txOutRefToPlutus,
  validatorFromPlutus,
 )

import HydraAuction.Onchain.Compiled qualified as O
import HydraAuction.Onchain.Types.AuctionTerms qualified as O
import HydraAuction.Onchain.Types.Scripts qualified as O
import HydraAuction.Onchain.Types.Tokens qualified as O

import HydraAuction.Offchain.Types.ScriptInfo (AuctionScriptInfo (..))
import HydraAuction.Offchain.Types.Scripts (
  AuctionEscrowV (..),
  AuctionMetadataV (..),
  BidderDepositV (..),
  FeeEscrowV (..),
  StandingBidV (..),
  auctionEscrowVToOnchain,
  auctionMetadataVToOnchain,
  feeEscrowVToOnchain,
  standingBidVToOnchain,
 )
import HydraAuction.Offchain.Types.Tokens (
  AuctionMp (..),
  auctionMpToOnchain,
 )

-- -------------------------------------------------------------------------
-- Auction script info
-- -------------------------------------------------------------------------
mkAuctionScriptInfo :: GYTxOutRef -> O.AuctionTerms -> AuctionScriptInfo
mkAuctionScriptInfo utxoNonce aTerms = AuctionScriptInfo {..}
  where
    si'AuctionTerms = aTerms
    --
    auctionMetadataSh =
      auctionMetadataVToOnchain $
        AuctionMetadataV auctionMetadataS
    --
    si'AuctionMp =
      AuctionMp $
        mkAuctionMpS auctionMetadataSh utxoNonce
    auctionId = auctionMpToOnchain si'AuctionMp
    --
    si'FeeEscrow =
      FeeEscrowV $
        mkFeeEscrowS aTerms
    feeEscrowSh = feeEscrowVToOnchain si'FeeEscrow
    --
    si'StandingBid =
      StandingBidV $
        mkStandingBidS auctionId aTerms
    standingBidSh = standingBidVToOnchain si'StandingBid
    --
    si'AuctionEscrow =
      AuctionEscrowV $
        mkAuctionEscrowS standingBidSh feeEscrowSh auctionId aTerms
    auctionEscrowSh = auctionEscrowVToOnchain si'AuctionEscrow
    --
    si'BidderDeposit =
      BidderDepositV $
        mkBidderDepositS auctionEscrowSh standingBidSh auctionId aTerms

-- -------------------------------------------------------------------------
-- Auction escrow validator
-- -------------------------------------------------------------------------
mkAuctionEscrowS ::
  O.StandingBid'ScriptHash ->
  O.FeeEscrow'ScriptHash ->
  O.AuctionId ->
  O.AuctionTerms ->
  GYValidator 'PlutusV2
mkAuctionEscrowS sbsh fsh auctionId aTerms =
  validatorFromPlutus $
    O.mkAuctionEscrowC sbsh fsh auctionId aTerms

-- -------------------------------------------------------------------------
-- Auction metadata validator
-- -------------------------------------------------------------------------
auctionMetadataS :: GYValidator 'PlutusV2
auctionMetadataS = validatorFromPlutus O.auctionMetadataC

-- -------------------------------------------------------------------------
-- Auction token minting policy
-- -------------------------------------------------------------------------
mkAuctionMpS ::
  O.AuctionMetadata'ScriptHash ->
  GYTxOutRef ->
  GYMintingPolicy 'PlutusV2
mkAuctionMpS amsh utxoNonce =
  mintingPolicyFromPlutus $
    O.mkAuctionMpC amsh $
      txOutRefToPlutus utxoNonce

-- -------------------------------------------------------------------------
-- Bidder deposit validator
-- -------------------------------------------------------------------------
mkBidderDepositS ::
  O.AuctionEscrow'ScriptHash ->
  O.StandingBid'ScriptHash ->
  O.AuctionId ->
  O.AuctionTerms ->
  GYValidator 'PlutusV2
mkBidderDepositS aesh sbsh auctionId aTerms =
  validatorFromPlutus $
    O.mkBidderDepositC aesh sbsh auctionId aTerms

-- -------------------------------------------------------------------------
-- Fee escrow validator
-- -------------------------------------------------------------------------
mkFeeEscrowS ::
  O.AuctionTerms ->
  GYValidator 'PlutusV2
mkFeeEscrowS = validatorFromPlutus . O.mkFeeEscrowC

-------------------------------------------------------------------------
-- Standing bid validator
-- -------------------------------------------------------------------------
mkStandingBidS ::
  O.AuctionId ->
  O.AuctionTerms ->
  GYValidator 'PlutusV2
mkStandingBidS auctionId aTerms =
  validatorFromPlutus $
    O.mkStandingBidC auctionId aTerms
