module HydraAuction.OnChain.StandingBid (mkStandingBidValidator) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Interval (contains, interval)
import Plutus.V1.Ledger.Value (assetClass, assetClassValueOf)
import Plutus.V2.Ledger.Api (TxInfo, scriptContextTxInfo, txInInfoResolved, txInfoInputs, txInfoMint, txInfoOutputs, txInfoValidRange, txOutAddress)
import Plutus.V2.Ledger.Contexts (ScriptContext, ownHash)

-- Hydra auction imports
import HydraAuction.Addresses
import HydraAuction.OnChain.Common
import HydraAuction.OnChain.StateToken (StateTokenKind (..), stateTokenKindToTokenName)
import HydraAuction.Types

{-# INLINEABLE mkStandingBidValidator #-}
mkStandingBidValidator :: AuctionTerms -> StandingBidDatum -> StandingBidRedeemer -> ScriptContext -> Bool
mkStandingBidValidator terms datum redeemer context =
  -- All cases require single standing bid input
  case byAddress (scriptHashAddress $ ownHash context) $ txInInfoResolved <$> txInfoInputs info of
    [inputOut] -> case redeemer of
      MoveToHydra ->
        -- XXX: using strange check, cuz == for lists failed compilation of Plutus Tx
        length (txInfoOutputs info) == 1 -- Check that nothing changed in output
          && head (txInfoOutputs info) == inputOut
          && nothingForged info
      NewBid ->
        checkCorrectNewBidOutput inputOut
          && nothingForged info
          && traceIfFalse
            "Wrong interval for NewBid"
            (contains (interval 0 (biddingEnd terms)) (txInfoValidRange info))
      Cleanup ->
        -- XXX: interval is checked on burning
        checkExactlyOneVoucherBurned
          && checkOutputIsToSeller
    _ : _ -> traceError "More than one standing bid input"
    [] -> traceError "Impossible happened: no inputs for staning bid validator"
  where
    info :: TxInfo
    info = scriptContextTxInfo context
    validNewBid :: StandingBidState -> StandingBidState -> Bool
    validNewBid oldBid (Bid newBidTerms) =
      case oldBid of
        Bid oldBidTerms ->
          traceIfFalse "Bid increment is not greater than minimumBidIncrement" $
            bidAmount oldBidTerms + minimumBidIncrement terms <= bidAmount newBidTerms
        NoBid ->
          traceIfFalse "Bid is not greater than startingBid" $
            startingBid terms <= bidAmount newBidTerms
    validNewBid _ NoBid = False
    checkCorrectNewBidOutput inputOut = case byAddress (scriptHashAddress $ ownHash context) $ txInfoOutputs info of
      [out] ->
        -- FIXME: Check bidder has right to make a bid
        traceIfFalse "Output is not into standing bid" $
          txOutAddress out == scriptHashAddress (ownHash context)
            && checkValidNewBid out
      _ -> traceError "Not exactly one ouput"
      where
        checkValidNewBid out =
          let inBid = standingBidState <$> decodeOutputDatum info inputOut
              outBid = standingBidState <$> decodeOutputDatum info out
           in case validNewBid <$> inBid <*> outBid of
                Just x -> traceIfFalse "Incorrect bid" x
                Nothing -> traceError "Incorrect encoding for input or output datum"
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
