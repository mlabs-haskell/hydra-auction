module HydraAuction.Onchain.Validators.AuctionEscrow (
  validator,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (geq)
import PlutusLedgerApi.V2 (
  ScriptContext (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
 )
import PlutusLedgerApi.V2.Contexts (
  txSignedBy,
  valuePaidTo,
 )

import HydraAuction.Error.Onchain.Validators.AuctionEscrow (
  AuctionEscrow'Error (..),
 )
import HydraAuction.Onchain.Lib.Error (eCode, err, errMaybe)
import HydraAuction.Onchain.Lib.PlutusTx (
  lovelaceValueOf,
  onlyOneInputFromAddress,
  parseInlineDatum,
 )
import HydraAuction.Onchain.Types.AuctionEscrowState (
  AuctionEscrowState (..),
  validateAuctionEscrowTransitionToAuctionConcluded,
  validateAuctionEscrowTransitionToStartBidding,
 )
import HydraAuction.Onchain.Types.AuctionTerms (
  AuctionTerms (..),
  auctionLotValue,
  biddingPeriod,
  cleanupPeriod,
  penaltyPeriod,
  purchasePeriod,
  totalAuctionFees,
 )
import HydraAuction.Onchain.Types.BidTerms (
  BidTerms (..),
  sellerPayout,
  validateBidTerms,
 )
import HydraAuction.Onchain.Types.BidderInfo (
  BidderInfo (..),
 )
import HydraAuction.Onchain.Types.Redeemers (
  AuctionEscrow'Redeemer (..),
 )
import HydraAuction.Onchain.Types.Scripts (
  FeeEscrow'ScriptHash (..),
  StandingBid'ScriptHash (..),
  findAuctionEscrowOwnInput,
  findAuctionEscrowTxOutAtAddr,
  findStandingBidInputAtSh,
  findStandingBidTxOutAtSh,
  valuePaidToFeeEscrow,
 )
import HydraAuction.Onchain.Types.StandingBidState (
  StandingBidState (..),
 )
import HydraAuction.Onchain.Types.Tokens (
  AuctionId (..),
  allAuctionTokensBurned,
  hasStandingBidToken,
 )

-- -------------------------------------------------------------------------
-- Validator
-- -------------------------------------------------------------------------
validator ::
  StandingBid'ScriptHash ->
  FeeEscrow'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  AuctionEscrowState ->
  AuctionEscrow'Redeemer ->
  ScriptContext ->
  Bool
validator sbsh fsh auctionId aTerms aState redeemer context =
  ownInputIsOnlyInputFromOwnScript
    && redeemerChecksPassed
  where
    TxInfo {..} = scriptContextTxInfo context
    --
    -- There should only be one auction escrow input.
    ownInputIsOnlyInputFromOwnScript =
      onlyOneInputFromAddress ownAddress txInfoInputs
        `err` $(eCode AuctionEscrow'Error'TooManyOwnScriptInputs)
    --
    -- The validator's own input should exist and
    -- it should contain an auction token.
    ownInput =
      txInInfoResolved
        $ findAuctionEscrowOwnInput auctionId context
        `errMaybe` $(eCode AuctionEscrow'Error'MissingAuctionEscrowInput)
    ownAddress = txOutAddress ownInput
    --
    -- Branching checks based on the redeemer used.
    redeemerChecksPassed =
      case redeemer of
        StartBidding ->
          checkSB sbsh auctionId aTerms aState context ownInput
        BidderBuys ->
          checkBB sbsh fsh auctionId aTerms aState context ownInput
        SellerReclaims ->
          checkSR fsh auctionId aTerms aState context ownInput
        CleanupAuction ->
          checkCA auctionId aTerms aState context ownInput
--
{-# INLINEABLE validator #-}

-- -------------------------------------------------------------------------
-- Start the bidding process
-- -------------------------------------------------------------------------

checkSB ::
  StandingBid'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  AuctionEscrowState ->
  ScriptContext ->
  TxOut ->
  Bool
checkSB sbsh auctionId aTerms oldAState context ownInput =
  auctionStateTransitionIsValid
    && initialBidStateIsEmpty
    && noTokensAreMintedOrBurned
    && validityIntervalIsCorrect
    && txSignedBySeller
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    AuctionTerms {..} = aTerms
    ownAddress = txOutAddress ownInput
    --
    -- The auction state should transition from AnnouncedAuction
    -- to StartBidding.
    auctionStateTransitionIsValid =
      validateAuctionEscrowTransitionToStartBidding oldAState newAState
        `err` $(eCode AuctionEscrow'SB'Error'InvalidAuctionStateTransition)
    --
    -- The standing bid state should be initialized without bid terms.
    initialBidStateIsEmpty =
      (initialBidState == StandingBidState Nothing)
        `err` $(eCode AuctionEscrow'SB'Error'InitialBidStateInvalid)
    --
    -- No tokens should be minted or burned.
    noTokensAreMintedOrBurned =
      (txInfoMint == mempty)
        `err` $(eCode AuctionEscrow'SB'Error'UnexpectedTokensMintedBurned)
    --
    -- This redeemer can only be used during the bidding period.
    validityIntervalIsCorrect =
      (biddingPeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode AuctionEscrow'SB'Error'IncorrectValidityInterval)
    --
    -- The transaction should be signed by the seller.
    txSignedBySeller =
      txSignedBy txInfo at'SellerPkh
        `err` $(eCode AuctionEscrow'SB'Error'MissingSellerSignature)
    --
    -- The auction escrow output contains a datum that can be
    -- decoded as an auction escrow state.
    newAState :: AuctionEscrowState
    newAState =
      parseInlineDatum auctionEscrowOutput
        `errMaybe` $(eCode AuctionEscrow'SB'Error'UndecodedAuctionEscrowDatum)
    --
    -- There is an output at the auction escrow validator
    -- containing the auction token.
    auctionEscrowOutput =
      findAuctionEscrowTxOutAtAddr auctionId ownAddress txInfoOutputs
        `errMaybe` $(eCode AuctionEscrow'SB'Error'MissingAuctionEscrowOutput)
    --
    -- The standing bid output contains a datum that can be
    -- decoded as a standing bid state.
    initialBidState :: StandingBidState
    initialBidState =
      parseInlineDatum standingBidOutput
        `errMaybe` $(eCode AuctionEscrow'SB'Error'UndecodedInitialBid)
    --
    -- There is an output at the standing bid validator
    -- containing the standing bid token.
    standingBidOutput =
      findStandingBidTxOutAtSh auctionId sbsh txInfoOutputs
        `errMaybe` $(eCode AuctionEscrow'SB'Error'MissingStandingBidOutput)

--
{-# INLINEABLE checkSB #-}

-- -------------------------------------------------------------------------
-- Bidder buys auction lot
-- -------------------------------------------------------------------------

checkBB ::
  StandingBid'ScriptHash ->
  FeeEscrow'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  AuctionEscrowState ->
  ScriptContext ->
  TxOut ->
  Bool
checkBB sbsh fsh auctionId aTerms oldAState context ownInput =
  auctionStateTransitionIsValid
    && auctionEscrowOutputContainsStandingBidToken
    && bidTermsAreValid
    && auctionLotPaidToBidder
    && paymentToSellerIsCorrect
    && paymentToFeeEscrowIsCorrect
    && noTokensAreMintedOrBurned
    && validityIntervalIsCorrect
    && bidderConsents
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    AuctionTerms {..} = aTerms
    AuctionId {..} = auctionId
    ownAddress = txOutAddress ownInput
    --
    -- The auction state should transition from StartBidding
    -- to AuctionConcluded.
    auctionStateTransitionIsValid =
      validateAuctionEscrowTransitionToAuctionConcluded oldAState newAState
        `err` $(eCode AuctionEscrow'BB'Error'InvalidAuctionStateTransition)
    --
    -- The auction escrow output contains the standing bid token
    -- in addition to the auction token.
    auctionEscrowOutputContainsStandingBidToken =
      hasStandingBidToken auctionId auctionEscrowOutput
        `err` $(eCode AuctionEscrow'BB'Error'AuctionEscrowOutputMissingTokens)
    --
    -- The bid terms in the standing bid input are valid.
    bidTermsAreValid =
      validateBidTerms aTerms auctionCs bidTerms
        `err` $(eCode AuctionEscrow'BB'Error'BidTermsInvalid)
    --
    -- The auction lot is paid to the winning bidder, who is buying it.
    auctionLotPaidToBidder =
      (paymentToBidder `geq` auctionLotValue aTerms)
        `err` $(eCode AuctionEscrow'BB'Error'AuctionLotNotPaidToBidder)
    paymentToBidder = valuePaidTo txInfo bi'BidderPkh
    --
    -- The seller receives the proceeds of the auction.
    paymentToSellerIsCorrect =
      (paymentToSeller >= sellerPayout aTerms bidTerms)
        `err` $(eCode AuctionEscrow'BB'Error'SellerPaymentIncorrect)
    paymentToSeller =
      lovelaceValueOf $ valuePaidTo txInfo at'SellerPkh
    --
    -- The total auction fees are sent to the fee escrow validator.
    paymentToFeeEscrowIsCorrect =
      (paymentToFeeEscrow >= totalAuctionFees aTerms)
        `err` $(eCode AuctionEscrow'BB'Error'PaymentToFeeEscrowIncorrect)
    paymentToFeeEscrow =
      lovelaceValueOf $ valuePaidToFeeEscrow txInfo fsh
    --
    -- No tokens are minted or burned.
    noTokensAreMintedOrBurned =
      (txInfoMint == mempty)
        `err` $(eCode AuctionEscrow'BB'Error'UnexpectedTokensMintedBurned)
    --
    -- This redeemer can only be used during the purchase period.
    validityIntervalIsCorrect =
      (purchasePeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode AuctionEscrow'BB'Error'IncorrectValidityInterval)
    --
    -- The bidder deposit's bidder consents to the transaction either
    -- explicitly by signing the transaction or
    -- implicitly by receiving the bid deposit ADA.
    bidderConsents =
      txSignedBy txInfo bi'BidderPkh
        `err` $(eCode AuctionEscrow'BB'Error'NoBidderConsent)
    --
    -- The auction escrow output contains a datum that can be
    -- decoded as an auction escrow state.
    newAState :: AuctionEscrowState
    newAState =
      parseInlineDatum auctionEscrowOutput
        `errMaybe` $(eCode AuctionEscrow'BB'Error'UndecodedAuctionEscrowDatum)
    --
    -- The auction escrow output exists and contains
    -- the auction token.
    auctionEscrowOutput =
      findAuctionEscrowTxOutAtAddr auctionId ownAddress txInfoOutputs
        `errMaybe` $(eCode AuctionEscrow'BB'Error'MissingAuctionEscrowOutput)
    --
    -- The standing bid contains bid terms.
    --
    -- The standing bid contains bid terms.
    bidTerms :: BidTerms
    bidTerms@BidTerms {..} =
      standingBidState bidState
        `errMaybe` $(eCode AuctionEscrow'BB'Error'EmptyStandingBid)
    --
    BidderInfo {..} = bt'Bidder
    --
    -- The standing bid input contains a datum that can be decoded
    -- as a standing bid state.
    bidState :: StandingBidState
    bidState =
      parseInlineDatum standingBidInput
        `errMaybe` $(eCode AuctionEscrow'BB'Error'UndecodedStandingBid)
    --
    -- There is a standing bid input that contains
    -- the standing bid token.
    standingBidInput =
      txInInfoResolved
        $ findStandingBidInputAtSh auctionId sbsh txInfoInputs
        `errMaybe` $(eCode AuctionEscrow'BB'Error'MissingStandingBidOutput)

--
{-# INLINEABLE checkBB #-}

-- -------------------------------------------------------------------------
-- Seller reclaims auction lot
-- -------------------------------------------------------------------------

checkSR ::
  FeeEscrow'ScriptHash ->
  AuctionId ->
  AuctionTerms ->
  AuctionEscrowState ->
  ScriptContext ->
  TxOut ->
  Bool
checkSR fsh auctionId aTerms oldAState context ownInput =
  auctionStateTransitionIsValid
    && auctionEscrowOutputContainsStandingBidToken
    && auctionLotReturnedToSeller
    && paymentToFeeEscrowIsCorrect
    && noTokensAreMintedOrBurned
    && validityIntervalIsCorrect
    && sellerConsents
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    AuctionTerms {..} = aTerms
    ownAddress = txOutAddress ownInput
    --
    -- The auction state should transition from StartBidding
    -- to AuctionConcluded.
    auctionStateTransitionIsValid =
      validateAuctionEscrowTransitionToAuctionConcluded oldAState newAState
        `err` $(eCode AuctionEscrow'SR'Error'InvalidAuctionStateTransition)
    --
    -- The auction escrow output contains the standing bid token
    -- in addition to the auction token.
    auctionEscrowOutputContainsStandingBidToken =
      hasStandingBidToken auctionId auctionEscrowOutput
        `err` $(eCode AuctionEscrow'SR'Error'AuctionEscrowOutputMissingTokens)
    --
    -- The auction lot is returned to the seller.
    auctionLotReturnedToSeller =
      (paymentToSeller `geq` auctionLotValue aTerms)
        `err` $(eCode AuctionEscrow'SR'Error'PaymentToSellerIncorrect)
    paymentToSeller = valuePaidTo txInfo at'SellerPkh
    --
    -- The total auction fees are sent to the fee escrow validator.
    paymentToFeeEscrowIsCorrect =
      (paymentToFeeEscrow >= totalAuctionFees aTerms)
        `err` $(eCode AuctionEscrow'SR'Error'PaymentToFeeEscrowIncorrect)
    paymentToFeeEscrow =
      lovelaceValueOf $ valuePaidToFeeEscrow txInfo fsh
    --
    -- No tokens are minted or burned.
    noTokensAreMintedOrBurned =
      (txInfoMint == mempty)
        `err` $(eCode AuctionEscrow'SR'Error'UnexpectedTokensMintedBurned)
    --
    -- This redeemer can only be used during the penalty period.
    validityIntervalIsCorrect =
      (penaltyPeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode AuctionEscrow'SR'Error'IncorrectValidityInterval)
    --
    -- The seller consents to the transaction either
    -- explicitly by signing it or
    -- implicitly by receiving the bid deposit ADA.
    sellerConsents =
      txSignedBy txInfo at'SellerPkh
        `err` $(eCode AuctionEscrow'SR'Error'NoSellerConsent)
    --
    -- The auction escrow output contains a datum that can be
    -- decoded as an auction escrow state.
    newAState :: AuctionEscrowState
    newAState =
      parseInlineDatum auctionEscrowOutput
        `errMaybe` $(eCode AuctionEscrow'SR'Error'UndecodedAuctionEscrowDatum)
    --
    -- There is an auction escrow output that contains
    -- the auction token.
    auctionEscrowOutput =
      findAuctionEscrowTxOutAtAddr auctionId ownAddress txInfoOutputs
        `errMaybe` $(eCode AuctionEscrow'SR'Error'MissingAuctionEscrowOutput)
--
{-# INLINEABLE checkSR #-}

-- -------------------------------------------------------------------------
-- Cleanup auction
-- -------------------------------------------------------------------------

checkCA ::
  AuctionId ->
  AuctionTerms ->
  AuctionEscrowState ->
  ScriptContext ->
  TxOut ->
  Bool
checkCA auctionId aTerms aState context ownInput =
  auctionIsConcluded
    && auctionEscrowInputContainsStandingBidToken
    && auctionTokensAreBurnedExactly
    && validityIntervalIsCorrect
    && sellerConsents
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    AuctionTerms {..} = aTerms
    --
    -- The auction is concluded.
    auctionIsConcluded =
      (aState == AuctionConcluded)
        `err` $(eCode AuctionEscrow'CA'Error'AuctionIsNotConcluded)
    --
    -- The auction escrow output contains the standing bid token
    -- in addition to the auction token.
    auctionEscrowInputContainsStandingBidToken =
      hasStandingBidToken auctionId ownInput
        `err` $(eCode AuctionEscrow'CA'Error'AuctionEscrowInputMissingTokens)
    --
    -- The auction state, auction metadata, and standing bid tokens
    -- of the auction should all be burned.
    -- No other tokens should be minted or burned.
    auctionTokensAreBurnedExactly =
      (txInfoMint == allAuctionTokensBurned auctionId)
        `err` $(eCode AuctionEscrow'CA'Error'AuctionTokensNotBurnedExactly)
    --
    -- This redeemer can only be used during the cleanup period.
    validityIntervalIsCorrect =
      (cleanupPeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode AuctionEscrow'CA'Error'IncorrectValidityInterval)
    --
    -- The seller consents to the transaction either
    -- explicitly by signing it or
    -- implicitly by receiving the bid deposit ADA.
    sellerConsents =
      txSignedBy txInfo at'SellerPkh
        `err` $(eCode AuctionEscrow'CA'Error'NoSellerConsent)
--
{-# INLINEABLE checkCA #-}
