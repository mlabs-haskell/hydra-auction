{-# OPTIONS_GHC -fno-specialise #-}

module HydraAuction.OnChain.StandingBid (
  mkStandingBidValidator,
  validNewBidTerms,
) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Interval (contains, to)
import Plutus.V1.Ledger.Value (assetClass, assetClassValueOf)
import Plutus.V2.Ledger.Api (
  TxInfo,
  TxOut (..),
  scriptContextTxInfo,
  txInInfoResolved,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoValidRange,
  txOutAddress,
 )
import Plutus.V2.Ledger.Contexts (ScriptContext, ownHash)

-- Hydra imporst
import Hydra.Contract.Head (hasPT)

-- Hydra auction imports
import HydraAuction.Addresses (VoucherCS (..))
import HydraAuction.OnChain.Common (byAddress, decodeOutputDatum, isNotAdaOnlyOutput, nothingForged)
import HydraAuction.OnChain.StateToken (StateTokenKind (..), stateTokenKindToTokenName)
import HydraAuction.Types (
  AuctionTerms (..),
  BidTerms (..),
  StandingBidDatum (..),
  StandingBidRedeemer (..),
  StandingBidState (..),
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
              (contains (to (biddingEnd terms)) (txInfoValidRange info))
        Cleanup ->
          -- XXX: interval is checked on burning
          checkExactlyOneVoucherBurned
            && checkOutputIsToSeller
    _ : _ -> traceError "More than one standing bid input"
    [] -> traceError "Impossible happened: no inputs for staning bid validator"
  where
    info :: TxInfo
    info = scriptContextTxInfo context
    standingBidAddress = scriptHashAddress $ ownHash context
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
            (txOutAddress out == scriptHashAddress (ownHash context))
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
