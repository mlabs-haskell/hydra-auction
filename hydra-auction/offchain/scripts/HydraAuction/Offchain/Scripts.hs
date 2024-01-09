module HydraAuction.Offchain.Scripts (
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

import HydraAuction.Onchain.Compiled
import HydraAuction.Onchain.Types.AuctionTerms
import HydraAuction.Onchain.Types.Tokens (AuctionId (..))

-- -------------------------------------------------------------------------
-- Auction metadata validator
-- -------------------------------------------------------------------------
auctionMetadataS :: GYValidator 'PlutusV2
auctionMetadataS = validatorFromPlutus auctionMetadataC

-- -------------------------------------------------------------------------
-- Auction escrow validator
-- -------------------------------------------------------------------------
mkAuctionEscrowS ::
  StandingBid'ScriptHash ->
  FeeEscrow'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  GYValidator 'PlutusV2
mkAuctionEscrowS sbsh fsh auctionId aTerms =
  validatorFromPlutus $
    mkAuctionEscrowC sbsh fsh auctionId aTerms

-- -------------------------------------------------------------------------
-- Auction token minting policy
-- -------------------------------------------------------------------------
mkAuctionMpS ::
  AuctionMetadata'ScriptHash ->
  GYTxOutRef ->
  GYMintingPolicy 'PlutusV2
mkAuctionMpS amsh utxoNonce =
  mintingPolicyFromPlutus $
    mkAuctionMpC amsh $
      txOutRefToPlutus utxoNonce

-- -------------------------------------------------------------------------
-- Bidder deposit validator
-- -------------------------------------------------------------------------
mkBidderDepositS ::
  AuctionEscrow'ScriptHash ->
  StandingBid'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  GYValidator 'PlutusV2
mkBidderDepositS aesh sbsh auctionId aTerms =
  validatorFromPlutus $
    mkBidderDepositC aesh sbsh auctionId aTerms

-- -------------------------------------------------------------------------
-- Fee escrow validator
-- -------------------------------------------------------------------------
mkFeeEscrowS ::
  AuctionTerms ->
  GYValidator 'PlutusV2
mkFeeEscrowS = validatorFromPlutus . mkFeeEscrowC

-------------------------------------------------------------------------
-- Standing bid validator
-- -------------------------------------------------------------------------
mkStandingBidS ::
  AuctionId ->
  AuctionTerms ->
  GYValidator 'PlutusV2
mkStandingBidS auctionId aTerms =
  validatorFromPlutus $
    mkStandingBidC auctionId aTerms
