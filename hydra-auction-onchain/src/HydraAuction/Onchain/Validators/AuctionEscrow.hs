module HydraAuction.Onchain.Validators.AuctionEscrow (
  validator,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V2 (
  PubKeyHash,
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
import HydraAuction.Onchain.Types.AuctionState (
  AuctionEscrowState (..),
  StandingBidState (..),
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
  validateBuyer,
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
import HydraAuction.Onchain.Types.Tokens (
  AuctionID (..),
  allAuctionTokensBurned,
  hasStandingBidToken,
 )

-- -------------------------------------------------------------------------
-- Validator
-- -------------------------------------------------------------------------
validator ::
  StandingBid'ScriptHash ->
  FeeEscrow'ScriptHash ->
  AuctionID ->
  AuctionTerms ->
  AuctionEscrowState ->
  AuctionEscrow'Redeemer ->
  ScriptContext ->
  Bool
validator sbsh fsh auctionID aTerms aState redeemer context =
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
      txInInfoResolved $
        findAuctionEscrowOwnInput auctionID context
          `errMaybe` $(eCode AuctionEscrow'Error'MissingAuctionEscrowInput)
    ownAddress = txOutAddress ownInput
    --
    -- Branching checks based on the redeemer used.
    redeemerChecksPassed =
      case redeemer of
        StartBidding ->
          checkSB sbsh auctionID aTerms aState context ownInput
        BidderBuys buyer ->
          checkBB sbsh fsh auctionID aTerms aState context ownInput buyer
        SellerReclaims ->
          checkSR fsh auctionID aTerms aState context ownInput
        CleanupAuction ->
          checkCA auctionID aTerms aState context ownInput
--
{-# INLINEABLE validator #-}

-- -------------------------------------------------------------------------
-- Start the bidding process
-- -------------------------------------------------------------------------

checkSB ::
  StandingBid'ScriptHash ->
  AuctionID ->
  AuctionTerms ->
  AuctionEscrowState ->
  ScriptContext ->
  TxOut ->
  Bool
checkSB sbsh auctionID aTerms oldAState context ownInput =
  auctionStateTransitionIsValid
    && initialBidStateIsEmpty
    && validityIntervalIsCorrect
    && txSignedBySeller
    && noTokensAreMintedOrBurned
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
    -- No tokens should be minted or burned.
    noTokensAreMintedOrBurned =
      (txInfoMint == mempty)
        `err` $(eCode AuctionEscrow'SB'Error'UnexpectedTokensMintedBurned)
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
      findAuctionEscrowTxOutAtAddr auctionID ownAddress txInfoOutputs
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
      findStandingBidTxOutAtSh auctionID sbsh txInfoOutputs
        `errMaybe` $(eCode AuctionEscrow'SB'Error'MissingStandingBidOutput)

--
{-# INLINEABLE checkSB #-}

-- -------------------------------------------------------------------------
-- Bidder buys auction lot
-- -------------------------------------------------------------------------

checkBB ::
  StandingBid'ScriptHash ->
  FeeEscrow'ScriptHash ->
  AuctionID ->
  AuctionTerms ->
  AuctionEscrowState ->
  ScriptContext ->
  TxOut ->
  PubKeyHash ->
  Bool
checkBB sbsh fsh auctionID aTerms oldAState context ownInput buyer =
  auctionStateTransitionIsValid
    && auctionEscrowOutputContainsStandingBidToken
    && bidTermsAreValid
    && buyerIsWinningBidder
    && auctionLotPaidToBuyer
    && paymentToSellerIsCorrect
    && paymentToFeeEscrowIsCorrect
    && validityIntervalIsCorrect
    && noTokensAreMintedOrBurned
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    AuctionTerms {..} = aTerms
    AuctionID aid = auctionID
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
      hasStandingBidToken auctionID auctionEscrowOutput
        `err` $(eCode AuctionEscrow'BB'Error'AuctionEscrowOutputMissingTokens)
    --
    -- The bid terms in the standing bid input are valid.
    bidTermsAreValid =
      validateBidTerms aTerms aid bidTerms
        `err` $(eCode AuctionEscrow'BB'Error'BidTermsInvalid)
    --
    -- The winning bidder is buying the auction lot.
    buyerIsWinningBidder =
      validateBuyer bidTerms buyer
        `err` $(eCode AuctionEscrow'BB'Error'InvalidBuyer)
    --
    -- The auction lot is paid to the winning bidder, who is buying it.
    auctionLotPaidToBuyer =
      (valuePaidTo txInfo buyer == auctionLotValue aTerms)
        `err` $(eCode AuctionEscrow'BB'Error'AuctionLotNotPaidToBuyer)
    --
    -- The seller receives the proceeds of the auction.
    paymentToSellerIsCorrect =
      (paymentToSeller == sellerPayout aTerms bidTerms)
        `err` $(eCode AuctionEscrow'BB'Error'SellerPaymentIncorrect)
    paymentToSeller =
      lovelaceValueOf $ valuePaidTo txInfo at'SellerPkh
    --
    -- The total auction fees are sent to the fee escrow validator.
    paymentToFeeEscrowIsCorrect =
      (paymentToFeeEscrow == totalAuctionFees aTerms)
        `err` $(eCode AuctionEscrow'BB'Error'PaymentToFeeEscrowIncorrect)
    paymentToFeeEscrow =
      lovelaceValueOf $ valuePaidToFeeEscrow txInfo fsh
    --
    -- This redeemer can only be used during the purchase period.
    validityIntervalIsCorrect =
      (purchasePeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode AuctionEscrow'BB'Error'IncorrectValidityInterval)
    --
    -- No tokens are minted or burned.
    noTokensAreMintedOrBurned =
      (txInfoMint == mempty)
        `err` $(eCode AuctionEscrow'BB'Error'UnexpectedTokensMintedBurned)
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
      findAuctionEscrowTxOutAtAddr auctionID ownAddress txInfoOutputs
        `errMaybe` $(eCode AuctionEscrow'BB'Error'MissingAuctionEscrowOutput)
    --
    -- The standing bid contains bid terms.
    bidTerms :: BidTerms
    bidTerms =
      standingBidState bidState
        `errMaybe` $(eCode AuctionEscrow'BB'Error'EmptyStandingBid)
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
      txInInfoResolved $
        findStandingBidInputAtSh auctionID sbsh txInfoInputs
          `errMaybe` $(eCode AuctionEscrow'BB'Error'MissingStandingBidOutput)

--
{-# INLINEABLE checkBB #-}

-- -------------------------------------------------------------------------
-- Seller reclaims auction lot
-- -------------------------------------------------------------------------

checkSR ::
  FeeEscrow'ScriptHash ->
  AuctionID ->
  AuctionTerms ->
  AuctionEscrowState ->
  ScriptContext ->
  TxOut ->
  Bool
checkSR fsh auctionID aTerms oldAState context ownInput =
  auctionStateTransitionIsValid
    && auctionEscrowOutputContainsStandingBidToken
    && auctionLotReturnedToSeller
    && paymentToFeeEscrowIsCorrect
    && validityIntervalIsCorrect
    && noTokensAreMintedOrBurned
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
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
      hasStandingBidToken auctionID auctionEscrowOutput
        `err` $(eCode AuctionEscrow'SR'Error'AuctionEscrowOutputMissingTokens)
    --
    -- The auction lot is returned to the seller.
    auctionLotReturnedToSeller =
      sellerReclaimedAuctionLot aTerms txInfo
        `err` $(eCode AuctionEscrow'SR'Error'PaymentToSellerIncorrect)
    --
    -- The total auction fees are sent to the fee escrow validator.
    paymentToFeeEscrowIsCorrect =
      (paymentToFeeEscrow == totalAuctionFees aTerms)
        `err` $(eCode AuctionEscrow'SR'Error'PaymentToFeeEscrowIncorrect)
    paymentToFeeEscrow =
      lovelaceValueOf $ valuePaidToFeeEscrow txInfo fsh
    --
    -- This redeemer can only be used during the penalty period.
    validityIntervalIsCorrect =
      (penaltyPeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode AuctionEscrow'SR'Error'IncorrectValidityInterval)
    --
    -- No tokens are minted or burned.
    noTokensAreMintedOrBurned =
      (txInfoMint == mempty)
        `err` $(eCode AuctionEscrow'SR'Error'UnexpectedTokensMintedBurned)
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
      findAuctionEscrowTxOutAtAddr auctionID ownAddress txInfoOutputs
        `errMaybe` $(eCode AuctionEscrow'SR'Error'MissingAuctionEscrowOutput)
--
{-# INLINEABLE checkSR #-}

-- The auction lot is returned to the seller.
sellerReclaimedAuctionLot :: AuctionTerms -> TxInfo -> Bool
sellerReclaimedAuctionLot aTerms@AuctionTerms {..} txInfo =
  valuePaidTo txInfo at'SellerPkh == auctionLotValue aTerms
--
{-# INLINEABLE sellerReclaimedAuctionLot #-}

-- -------------------------------------------------------------------------
-- Cleanup auction
-- -------------------------------------------------------------------------

checkCA ::
  AuctionID ->
  AuctionTerms ->
  AuctionEscrowState ->
  ScriptContext ->
  TxOut ->
  Bool
checkCA auctionID aTerms aState context ownInput =
  auctionIsConcluded
    && auctionEscrowInputContainsStandingBidToken
    && auctionTokensAreBurnedExactly
    && validityIntervalIsCorrect
  where
    TxInfo {..} = scriptContextTxInfo context
    --
    -- The auction is concluded.
    auctionIsConcluded =
      (aState == AuctionConcluded)
        `err` $(eCode AuctionEscrow'CA'Error'AuctionIsNotConcluded)
    --
    -- The auction escrow output contains the standing bid token
    -- in addition to the auction token.
    auctionEscrowInputContainsStandingBidToken =
      hasStandingBidToken auctionID ownInput
        `err` $(eCode AuctionEscrow'CA'Error'AuctionEscrowInputMissingTokens)
    --
    -- The auction state, auction metadata, and standing bid tokens
    -- of the auction should all be burned.
    -- No other tokens should be minted or burned.
    auctionTokensAreBurnedExactly =
      (txInfoMint == allAuctionTokensBurned auctionID)
        `err` $(eCode AuctionEscrow'CA'Error'AuctionTokensNotBurnedExactly)
    --
    -- This redeemer can only be used during the cleanup period.
    validityIntervalIsCorrect =
      (cleanupPeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode AuctionEscrow'CA'Error'IncorrectValidityInterval)
--
{-# INLINEABLE checkCA #-}
