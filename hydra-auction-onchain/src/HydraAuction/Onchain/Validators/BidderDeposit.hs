module HydraAuction.Onchain.Validators.BidderDeposit (
  validator,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V2 (
  ScriptContext (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
 )
import PlutusLedgerApi.V2.Contexts (
  findOwnInput,
  txSignedBy,
 )

import HydraAuction.Error.Onchain.Validators.BidderDeposit (
  BidderDeposit'Error (..),
 )
import HydraAuction.Onchain.Lib.Error (eCode, err, errMaybe, errMaybeFlip)
import HydraAuction.Onchain.Lib.PlutusTx (
  getSpentInputRedeemer,
  onlyOneInputFromAddress,
  parseInlineDatum,
  parseRedemeer,
 )
import HydraAuction.Onchain.Types.AuctionState (
  AuctionEscrowState (..),
  StandingBidState (..),
  bidderLost,
  bidderWon,
 )
import HydraAuction.Onchain.Types.AuctionTerms (
  AuctionTerms (..),
  cleanupPeriod,
  postBiddingPeriod,
 )
import HydraAuction.Onchain.Types.BidderInfo (
  BidderInfo (..),
 )
import HydraAuction.Onchain.Types.Redeemers (
  AuctionEscrow'Redeemer (..),
  BidderDeposit'Redeemer (..),
  isConcluding,
 )
import HydraAuction.Onchain.Types.Scripts (
  AuctionEscrow'ScriptHash,
  StandingBid'ScriptHash (..),
  findAuctionEscrowInputAtSh,
  findStandingBidInputAtSh,
 )
import HydraAuction.Onchain.Types.Tokens (
  AuctionId (..),
 )

-- -------------------------------------------------------------------------
-- Validator
-- -------------------------------------------------------------------------
validator ::
  AuctionEscrow'ScriptHash ->
  StandingBid'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  BidderInfo ->
  BidderDeposit'Redeemer ->
  ScriptContext ->
  Bool
validator aesh sbsh auctionId aTerms bInfo redeemer context =
  ownInputIsOnlyInputFromOwnScript
    && noTokensAreMintedOrBurned
    && redeemerChecksPassed
  where
    TxInfo {..} = scriptContextTxInfo context
    --
    -- There should only be one bidder deposit input.
    ownInputIsOnlyInputFromOwnScript =
      onlyOneInputFromAddress ownAddress txInfoInputs
        `err` $(eCode BidderDeposit'Error'TooManyOwnScriptInputs)
    --
    -- No tokens are minted or burned.
    noTokensAreMintedOrBurned =
      (txInfoMint == mempty)
        `err` $(eCode BidderDeposit'Error'UnexpectedMintOrBurn)
    --
    -- The validator's own input should exist.
    ownInput =
      txInInfoResolved $
        findOwnInput context
          `errMaybe` $(eCode BidderDeposit'Error'MissingOwnInput)
    ownAddress = txOutAddress ownInput
    --
    -- Branching checks based on the redeemer used.
    redeemerChecksPassed =
      case redeemer of
        DepositUsedToConcludeAuction ->
          checkCA aesh sbsh auctionId bInfo context
        DepositReclaimedByLoser ->
          checkBL sbsh auctionId aTerms bInfo context
        DepositReclaimedAuctionConcluded ->
          checkAC aesh auctionId aTerms bInfo context
        DepositCleanup ->
          checkDC aTerms bInfo context
--
{-# INLINEABLE validator #-}

-- -------------------------------------------------------------------------
-- Deposit used by winner
-- -------------------------------------------------------------------------

-- Deposit is used by the bidder who won the auction to buy the auction lot.
checkCA ::
  AuctionEscrow'ScriptHash ->
  StandingBid'ScriptHash ->
  AuctionId ->
  BidderInfo ->
  ScriptContext ->
  Bool
checkCA aesh sbsh auctionId bInfo context =
  auctionIsConcluding
    && bidderWonTheAuction
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    --
    -- The auction escrow input is being spent with
    -- a BidderBuys redeemer.
    auctionIsConcluding =
      isConcluding aRedeemer
        `err` $(eCode BidderDeposit'CA'Error'AuctionNotConcluding)
    --
    -- The bidder deposit's bidder won the auction.
    bidderWonTheAuction =
      bidderWon bidState bInfo
        `err` $(eCode BidderDeposit'CA'Error'BidderNotWinner)
    --
    -- The auction input's redeemer can be decoded
    -- as an auction escrow redeemer.
    aRedeemer :: AuctionEscrow'Redeemer
    aRedeemer =
      errMaybeFlip
        $(eCode BidderDeposit'CA'Error'UndecodedAuctionRedeemer)
        $ parseRedemeer =<< getSpentInputRedeemer txInfo auctionEscrowInput
    --
    -- There is an auction escrow input that contains
    -- the auction token.
    auctionEscrowInput =
      findAuctionEscrowInputAtSh auctionId aesh txInfoInputs
        `errMaybe` $(eCode BidderDeposit'CA'Error'MissingAuctionEscrowInput)
    --
    -- The standing bid input contains a datum that can be decoded
    -- as a standing bid state.
    bidState =
      parseInlineDatum standingBidInput
        `errMaybe` $(eCode BidderDeposit'CA'Error'UndecodedBidState)
    --
    -- There is a standing bid input that contains
    -- the standing bid token.
    standingBidInput =
      txInInfoResolved $
        findStandingBidInputAtSh auctionId sbsh txInfoInputs
          `errMaybe` $(eCode BidderDeposit'CA'Error'MissingStandingBidInput)
--
{-# INLINEABLE checkCA #-}

-- -------------------------------------------------------------------------
-- Deposit reclaimed by losing bidder
-- -------------------------------------------------------------------------

-- The bidder deposit is reclaimed by a bidder that did not win the auction.
checkBL ::
  StandingBid'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  BidderInfo ->
  ScriptContext ->
  Bool
checkBL sbsh auctionId aTerms bInfo context =
  bidderLostTheAuction
    && validityIntervalIsCorrect
    && bidderConsents
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    BidderInfo {..} = bInfo
    --
    -- The bidder deposit's bidder lost the auction.
    bidderLostTheAuction =
      bidderLost bidState bInfo
        `err` $(eCode BidderDeposit'BL'Error'BidderNotLoser)
    --
    -- This redeemer can only be used after the bidding period.
    validityIntervalIsCorrect =
      (postBiddingPeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode BidderDeposit'BL'Error'ValidityIntervalIncorrect)
    --
    -- The bidder deposit's bidder consents to the transcation either
    -- explictly by signing the transaction or
    -- implicitly by receiving the bid deposit ADA.
    bidderConsents =
      txSignedBy txInfo bi'BidderPkh
        `err` $(eCode BidderDeposit'BL'Error'NoBidderConsent)
    --
    -- The standing bid input contains a datum that can be decoded
    -- as a standing bid state.
    bidState :: StandingBidState
    bidState =
      parseInlineDatum standingBidInput
        `errMaybe` $(eCode BidderDeposit'BL'Error'UndecodedBidState)
    --
    -- There is a standing bid input that contains
    -- the standing bid token.
    standingBidInput =
      txInInfoResolved $
        findStandingBidInputAtSh auctionId sbsh txInfoInputs
          `errMaybe` $(eCode BidderDeposit'BL'Error'MissingStandingBidInput)
--
{-# INLINEABLE checkBL #-}

-- -------------------------------------------------------------------------
-- Deposit reclaimed by losing bidder
-- -------------------------------------------------------------------------

-- The bidder deposit is reclaimed by a bidder after the auction conclusion.
-- If the auction has concluded then the seller and the winning bidder
-- have already had an opportunity to claim
-- whichever deposits they are entitled to.
checkAC ::
  AuctionEscrow'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  BidderInfo ->
  ScriptContext ->
  Bool
checkAC aesh auctionId aTerms bInfo context =
  auctionIsConcluded
    && validityIntervalIsCorrect
    && bidderConsents
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    BidderInfo {..} = bInfo
    --
    -- The auction is concluded.
    auctionIsConcluded =
      (aState == AuctionConcluded)
        `err` $(eCode BidderDeposit'AC'Error'AuctionNotConcluded)
    --
    -- This redeemer can only be used after the bidding period.
    validityIntervalIsCorrect =
      (postBiddingPeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode BidderDeposit'AC'Error'ValidityIntervalIncorrect)
    --
    -- The bidder deposit's bidder consents to the transcation either
    -- explictly by signing the transaction or
    -- implicitly by receiving the bid deposit ADA.
    bidderConsents =
      txSignedBy txInfo bi'BidderPkh
        `err` $(eCode BidderDeposit'AC'Error'NoBidderConsent)
    --
    -- The auction escrow output contains a datum that can be
    -- decoded as an auction escrow state.
    aState :: AuctionEscrowState
    aState =
      parseInlineDatum auctionEscrowReferenceInput
        `errMaybe` $(eCode BidderDeposit'AC'Error'UndecodedAuctionState)
    --
    -- There is an auction escrow reference input that contains
    -- the auction token.
    auctionEscrowReferenceInput =
      txInInfoResolved $
        findAuctionEscrowInputAtSh auctionId aesh txInfoReferenceInputs
          `errMaybe` $(eCode BidderDeposit'AC'Error'MissingAuctionRefInput)
--
{-# INLINEABLE checkAC #-}

-- -------------------------------------------------------------------------
-- Deposit cleanup
-- -------------------------------------------------------------------------

-- If, for whatever reason, there are bidder deposits left during the
-- cleanup period, then whoever placed a deposit can freely reclaim it.
checkDC ::
  AuctionTerms ->
  BidderInfo ->
  ScriptContext ->
  Bool
checkDC aTerms bInfo context =
  validityIntervalIsCorrect
    && bidderConsents
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    BidderInfo {..} = bInfo
    --
    -- This redeemer can only be used during the cleanup period.
    validityIntervalIsCorrect =
      (cleanupPeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode BidderDeposit'DC'Error'ValidityIntervalIncorrect)
    --
    -- The bidder deposit's bidder consents to the transcation either
    -- explictly by signing the transaction or
    -- implicitly by receiving the bid deposit ADA.
    bidderConsents =
      txSignedBy txInfo bi'BidderPkh
        `err` $(eCode BidderDeposit'DC'Error'NoBidderConsent)
--
{-# INLINEABLE checkDC #-}
