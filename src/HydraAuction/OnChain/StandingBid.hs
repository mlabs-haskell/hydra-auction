{-# OPTIONS_GHC -fno-specialise #-}

module HydraAuction.OnChain.StandingBid (
  mkStandingBidValidator,
  validNewBidTerms,
) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports

import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (assetClass, assetClassValueOf)
import PlutusLedgerApi.V2 (txInInfoResolved)
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (..),
  TxInfo (..),
  findOwnInput,
 )
import PlutusLedgerApi.V2.Tx (
  TxOut (..),
  txOutAddress,
 )

-- Hydra imporst
import Hydra.Contract.Head (hasPT)

-- Hydra auction imports
import HydraAuction.Addresses (VoucherCS (..))
import HydraAuction.OnChain.Common (strictTo)
import HydraAuction.OnChain.StateToken (StateTokenKind (..), stateTokenKindToTokenName)
import HydraAuction.Types (
  AuctionTerms (..),
  BidTerms (..),
  StandingBidDatum (..),
  StandingBidRedeemer (..),
  StandingBidState (..),
 )
import HydraAuctionUtils.Plutus (
  byAddress,
  decodeOutputDatum,
  isNotAdaOnlyOutput,
  nothingForged,
 )

{-# INLINEABLE mkStandingBidValidator #-}
mkStandingBidValidator :: AuctionTerms -> StandingBidDatum -> StandingBidRedeemer -> ScriptContext -> Bool
mkStandingBidValidator terms datum redeemer context =
  -- All cases require single standing bid input
  case inOutsByAddress standingBidAddress of
    [standingBidInOut] ->
      case redeemer of
        MoveToHydra ->
          case filter isNotAdaOnlyOutput $ txInfoOutputs info of
            [standingBidOutput] ->
              -- Lot cannot be stolen, cuz we have only one output
              -- Hydra validator shoud check that datum is not changed
              -- in commited output
              -- Also validator checks that output address is correct
              -- The only thing we should check is Tx has right PT
              traceIfFalse "No Hydra Participation Token" $
                hasPT (hydraHeadId terms) standingBidOutput
            _ -> traceError "Wrong number of standing bid outputs"
            && nothingForged info
        NewBid ->
          checkCorrectNewBidOutput standingBidInOut
            && nothingForged info
            && traceIfFalse
              "Wrong interval for NewBid"
              (contains (strictTo (biddingEnd terms)) (txInfoValidRange info))
        Cleanup ->
          -- XXX: interval is checked on burning
          checkExactlyOneVoucherBurned
            && checkOutputIsToSeller
    _ : _ -> traceError "More than one standing bid input"
    [] -> traceError "Impossible happened: no inputs for staning bid validator"
  where
    info :: TxInfo
    info = scriptContextTxInfo context
    standingBidAddress = case findOwnInput context of
      Just x -> txOutAddress $ txInInfoResolved x
      Nothing -> traceError "Impossible happened"
    inOutsByAddress address =
      byAddress address $ txInInfoResolved <$> txInfoInputs info
    validNewBid :: StandingBidState -> StandingBidState -> Bool
    validNewBid (StandingBidState _oldApprovedBidders oldBid) (StandingBidState _newApprovedBidders newBid) =
      -- FIXME: disabled until M6
      -- traceIfFalse "Bidder not signed" (txSignedBy info (bidBidder newBidTerms))
      -- && traceIfFalse
      --   "Bidder is not approved"
      --   (bidBidder newBidTerms `elem` bidders oldApprovedBidders)
      -- traceIfFalse
      -- "Approved Bidders can not be modified"
      -- (oldApprovedBidders == newApprovedBidders)
      validNewBidTerms terms oldBid newBid
    checkCorrectNewBidOutput inputOut =
      case byAddress standingBidAddress $ txInfoOutputs info of
        [out] ->
          traceIfFalse
            "Output is not into standing bid"
            (txOutAddress out == standingBidAddress)
            && checkValidNewBid out
        _ -> traceError "Not exactly one ouput"
      where
        checkValidNewBid out =
          let inBid = standingBidState <$> decodeOutputDatum info inputOut
              outBid = standingBidState <$> decodeOutputDatum info out
           in case validNewBid <$> inBid <*> outBid of
                Just x -> traceIfFalse "Incorrect bid" x
                Nothing ->
                  traceError "Incorrect encoding for input or output datum"
    checkOutputIsToSeller = case txInfoOutputs info of
      [out] ->
        traceIfFalse
          "Output is not to seller"
          (txOutAddress out == pubKeyHashAddress (seller terms))
      _ -> traceError "Not exactly one ouput"
    checkExactlyOneVoucherBurned =
      traceIfFalse
        "Not exactly one voucher was burt during transaction"
        ( let cs = unVoucherCS $ standingBidVoucherCS datum
              voucherAC = assetClass cs (stateTokenKindToTokenName Voucher)
           in assetClassValueOf (txInfoMint info) voucherAC == -1
        )

{-# INLINEABLE validNewBidTerms #-}
validNewBidTerms :: AuctionTerms -> Maybe BidTerms -> Maybe BidTerms -> Bool
validNewBidTerms terms oldBid (Just newBidTerms) =
  case oldBid of
    Just oldBidTerms ->
      traceIfFalse "Bid increment is not greater than minimumBidIncrement" $
        bidAmount oldBidTerms + minimumBidIncrement terms <= bidAmount newBidTerms
    Nothing ->
      traceIfFalse "Bid is not greater than startingBid" $
        startingBid terms <= bidAmount newBidTerms
validNewBidTerms _ _ Nothing =
  traceIfFalse "Bid cannot be empty" False
