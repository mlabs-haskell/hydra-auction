module HydraAuction.Onchain.Validators.StandingBid (
  validator,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Interval (
  contains,
 )
import PlutusLedgerApi.V2 (
  ScriptContext (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
 )
import PlutusLedgerApi.V2.Contexts (
  txSignedBy,
 )

import HydraAuction.Error.Onchain.Validators.StandingBid (
  StandingBid'Error (..),
 )
import HydraAuction.Onchain.Lib.Error (eCode, err, errMaybe)
import HydraAuction.Onchain.Lib.PlutusTx (
  getSpentInputRedeemer,
  onlyOneInputFromAddress,
  parseInlineDatum,
  parseRedemeer,
 )
import HydraAuction.Onchain.Types.AuctionState (
  StandingBidState (..),
  validateNewBid,
 )
import HydraAuction.Onchain.Types.AuctionTerms (
  AuctionTerms (..),
  biddingPeriod,
 )
import HydraAuction.Onchain.Types.Scripts (
  AuctionEscrow'Redeemer,
  AuctionID (..),
  StandingBid'Redeemer (..),
  findAuctionEscrowTokenInput,
  findStandingBidOwnInput,
  findStandingBidTxOutAtAddr,
  isConcluding,
 )

-- -------------------------------------------------------------------------
-- Validator
-- -------------------------------------------------------------------------
validator ::
  AuctionID ->
  AuctionTerms ->
  StandingBidState ->
  StandingBid'Redeemer ->
  ScriptContext ->
  Bool
validator auctionID aTerms standingBidState redeemer context =
  ownInputIsOnlyInputFromOwnScript
    && noTokensAreMintedOrBurned
    && redeemerChecksPassed
  where
    TxInfo {..} = scriptContextTxInfo context
    --
    -- There should only be one standing bid input.
    ownInputIsOnlyInputFromOwnScript =
      onlyOneInputFromAddress ownAddress txInfoInputs
        `err` $(eCode StandingBid'Error'TooManyOwnScriptInputs)
    --
    -- There should be no tokens minted or burned.
    noTokensAreMintedOrBurned =
      (txInfoMint == mempty)
        `err` $(eCode StandingBid'Error'UnexpectedTokensMintedBurned)
    --
    -- The validator's own input should exist and
    -- it should contain a standing bid token.
    ownInput =
      txInInfoResolved $
        findStandingBidOwnInput auctionID context
          `errMaybe` $(eCode StandingBid'Error'MissingStandingBidInput)
    ownAddress = txOutAddress ownInput
    --
    redeemerChecksPassed =
      case redeemer of
        NewBid ->
          checkNB auctionID aTerms standingBidState context ownInput
        MoveToHydra ->
          checkMH aTerms context
        ConcludeAuction ->
          checkCA auctionID context
--
{-# INLINEABLE validator #-}

-- -------------------------------------------------------------------------
-- New bid
-- -------------------------------------------------------------------------
checkNB ::
  AuctionID ->
  AuctionTerms ->
  StandingBidState ->
  ScriptContext ->
  TxOut ->
  Bool
checkNB auctionID aTerms oldBidState context ownInput =
  bidStateTransitionIsValid
    && validityIntervalIsCorrect
  where
    TxInfo {..} = scriptContextTxInfo context
    AuctionID aid = auctionID
    ownAddress = txOutAddress ownInput
    --
    -- The transition from the old bid state to the new bid state
    -- should be valid.
    bidStateTransitionIsValid =
      validateNewBid aTerms aid oldBidState newBidState
        `err` $(eCode $ StandingBid'NB'Error'InvalidNewBidState [])
    --
    -- The transaction validity should end before the bidding end time.
    validityIntervalIsCorrect =
      (biddingPeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode StandingBid'NB'Error'IncorrectValidityInterval)

    --
    -- The standing bid output contains a datum that can be
    -- decoded as a standing bid state.
    newBidState :: StandingBidState
    newBidState =
      parseInlineDatum standingBidOutput
        `errMaybe` $(eCode StandingBid'NB'Error'FailedToDecodeNewBid)
    --
    -- There is an output at the standing bid validator
    -- containing the standing bid token.
    standingBidOutput =
      findStandingBidTxOutAtAddr auctionID ownAddress txInfoOutputs
        `errMaybe` $(eCode StandingBid'NB'Error'MissingStandingBidOutput)
--
{-# INLINEABLE checkNB #-}

-- -------------------------------------------------------------------------
-- Move to hydra
-- -------------------------------------------------------------------------
checkMH ::
  AuctionTerms ->
  ScriptContext ->
  Bool
checkMH aTerms@AuctionTerms {..} context =
  txSignedByAllDelegates
    && validityIntervalIsCorrect
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    --
    -- The transaction should be signed by all the delegates.
    txSignedByAllDelegates =
      all (txSignedBy txInfo) at'Delegates
        `err` $(eCode StandingBid'MH'Error'MissingDelegateSignatures)
    --
    -- The transaction validity should end before the bidding end time.
    validityIntervalIsCorrect =
      (biddingPeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode StandingBid'MH'Error'IncorrectValidityInterval)
--
{-# INLINEABLE checkMH #-}

-- -------------------------------------------------------------------------
-- Conclude auction
-- -------------------------------------------------------------------------
checkCA ::
  AuctionID ->
  ScriptContext ->
  Bool
checkCA auctionID context =
  auctionIsConcluding
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    --
    -- There should be an input with the auction state token that is
    -- being spent with the BidderBuys or SellerReclaims redeemer.
    -- Implicitly, this means that the auction is concluding
    -- with either the winning bidder buying the auction lot
    -- or the seller reclaiming it.
    auctionIsConcluding =
      isConcluding auctionRedeemer
        `err` $(eCode StandingBid'CA'Error'AuctionIsNotConcluding)
    --
    -- There is an input that contains the auction token
    -- and is being spent with an auction escrow redeemer.
    auctionRedeemer :: AuctionEscrow'Redeemer
    auctionRedeemer =
      mAuctionEscrowRedeemer
        `errMaybe` $(eCode StandingBid'CA'Error'InvalidAuctionTokenRedeemer)
    mAuctionEscrowRedeemer = do
      txin <- findAuctionEscrowTokenInput auctionID txInfoInputs
      r <- getSpentInputRedeemer txInfo txin
      parseRedemeer r
--
{-# INLINEABLE checkCA #-}
