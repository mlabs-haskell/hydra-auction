module HydraAuction.Onchain.Validators.StandingBid (
  validator,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1.Interval (
  contains,
 )
import PlutusLedgerApi.V1.Scripts (
  Datum (..),
 )
import PlutusLedgerApi.V1.Value (
  valueOf,
 )
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (..),
  TxInInfo (..),
  TxInfo (..),
  findOwnInput,
  txSignedBy,
 )
import PlutusLedgerApi.V2.Tx (
  OutputDatum (..),
  TxOut (..),
 )
import PlutusTx qualified

import HydraAuction.Error.Onchain.Validators.StandingBid (
  StandingBid'CA'Error (..),
  StandingBid'Error (..),
  StandingBid'MH'Error (..),
  StandingBid'NB'Error (..),
 )
import HydraAuction.Onchain.Lib.Error (eCode, err, errMaybe)
import HydraAuction.Onchain.Types.AuctionInfo (
  auctionTN,
  standingBidTN,
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
  AuctionID (..),
  StandingBid'Redeemer (..),
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
    -- (StandingBid01)
    -- There should only be one standing bid input.
    ownInputIsOnlyInputFromOwnScript =
      (length ownScriptInputs == 1)
        `err` $(eCode StandingBid'Error'TooManyOwnScriptInputs)
    ownScriptInputs =
      filter
        (\x -> ownAddress == txOutAddress (txInInfoResolved x))
        txInfoInputs
    --
    -- (StandingBid02)
    -- The standing bid input should exist.
    -- Note that this should always hold for a validator being executed
    -- with a Spending script purpose.
    ownInput =
      txInInfoResolved $
        findOwnInput context
          `errMaybe` $(eCode StandingBid'Error'MissingOwnInput)
    ownAddress = txOutAddress ownInput
    --
    -- (StandingBid03)
    -- There should be no tokens minted or burned.
    noTokensAreMintedOrBurned =
      (txInfoMint == mempty)
        `err` $(eCode StandingBid'Error'UnexpectedTokensMintedBurned)
    --
    redeemerChecksPassed =
      case redeemer of
        NewBid ->
          checkNewBid auctionID aTerms standingBidState context ownInput
        MoveToHydra ->
          checkMoveToHydra aTerms context
        ConcludeAuction ->
          checkConcludeAuction auctionID context ownInput
--
{-# INLINEABLE validator #-}

checkNewBid ::
  AuctionID ->
  AuctionTerms ->
  StandingBidState ->
  ScriptContext ->
  TxOut ->
  Bool
checkNewBid AuctionID {..} aTerms oldBidState context ownInput =
  ownInputContainsStandingBidToken
    && bidStateTransitionIsValid
    && validityIntervalIsCorrect
  where
    TxInfo {..} = scriptContextTxInfo context
    --
    -- (StandingBid_NB01)
    -- The standing bid input should contain the standing bid token.
    ownInputContainsStandingBidToken =
      (valueOf ownInputValue auctionID standingBidTN == 1)
        `err` $(eCode StandingBid'NB'Error'OwnInputMissingToken)
    ownInputValue = txOutValue ownInput
    --
    -- (StandingBid_NB02)
    -- The transition from the old bid state to the new bid state
    -- should be valid.
    bidStateTransitionIsValid =
      validateNewBid aTerms auctionID oldBidState newBidState
        `err` $(eCode $ StandingBid'NB'Error'InvalidNewBidState [])
    --
    -- (StandingBid_NB03)
    -- The transaction validity should end before the bidding end time.
    validityIntervalIsCorrect =
      (biddingPeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode StandingBid'NB'Error'IncorrectValidityInterval)
    --
    -- (StandingBid_NB04)
    -- The standing bid output's datum should be decodable
    -- as a standing bid state.
    newBidState :: StandingBidState
    newBidState =
      PlutusTx.fromBuiltinData
        (getDatum ownOutputDatum)
        `errMaybe` $(eCode StandingBid'NB'Error'FailedToDecodeNewBid)
    --
    -- (StandingBid_NB05)
    -- The standing bid output should contain an inline datum.
    ownOutputDatum
      | OutputDatum d <- txOutDatum ownOutput = d
      | otherwise =
          traceError $(eCode StandingBid'NB'Error'OwnOutputDatumNotInline)
    --
    -- (StandingBid_NB06)
    -- The standing bid output should exist.
    ownAddress = txOutAddress ownInput
    ownOutput =
      find
        ( \TxOut {..} ->
            ownAddress == txOutAddress
              && (valueOf txOutValue auctionID standingBidTN == 1)
        )
        txInfoOutputs
        `errMaybe` $(eCode StandingBid'NB'Error'MissingOwnOutput)

--
{-# INLINEABLE checkNewBid #-}

checkMoveToHydra ::
  AuctionTerms ->
  ScriptContext ->
  Bool
checkMoveToHydra aTerms@AuctionTerms {..} context =
  txSignedByAllDelegates
    && validityIntervalIsCorrect
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    --
    -- (StandingBid_MH01)
    -- The transaction should be signed by all the delegates.
    txSignedByAllDelegates =
      all (txSignedBy txInfo) at'Delegates
        `err` $(eCode StandingBid'MH'Error'MissingDelegateSignatures)
    --
    -- (StandingBid_MH02)
    -- The transaction validity should end before the bidding end time.
    validityIntervalIsCorrect =
      (biddingPeriod aTerms `contains` txInfoValidRange)
        `err` $(eCode StandingBid'MH'Error'IncorrectValidityInterval)
--
{-# INLINEABLE checkMoveToHydra #-}

checkConcludeAuction ::
  AuctionID ->
  ScriptContext ->
  TxOut ->
  Bool
checkConcludeAuction AuctionID {..} context ownInput =
  ownInputContainsStandingBidToken
    && auctionStateTokenIsSpent
  where
    TxInfo {..} = scriptContextTxInfo context
    --
    -- (StandingBid_CA01)
    -- The standing bid input should contain the standing bid token.
    ownInputContainsStandingBidToken =
      (valueOf ownInputValue auctionID standingBidTN == 1)
        `err` $(eCode StandingBid'CA'Error'OwnInputMissingToken)
    ownInputValue = txOutValue ownInput
    --
    -- (StandingBid_CA02)
    -- There should be input with the auction state token.
    -- Implicitly, this means that the auction is concluding
    -- with either the winning bidder buying the auction lot
    -- or the seller reclaiming it.
    auctionStateTokenIsSpent =
      any hasAuctionStateToken txInfoInputs
        `err` $(eCode StandingBid'CA'Error'MissingAuctionStateToken)
    hasAuctionStateToken x =
      valueOf (txOutValue $ txInInfoResolved x) auctionID auctionTN == 1
--
{-# INLINEABLE checkConcludeAuction #-}
