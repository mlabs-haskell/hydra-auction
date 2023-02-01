module HydraAuction.OnChain.StandingBid (mkStandingBidValidator) where

import PlutusTx.Prelude

import HydraAuction.Addresses
import HydraAuction.OnChain.Common
import HydraAuction.OnChain.StateToken (StateTokenKind (..), stateTokenKindToTokenName)
import HydraAuction.Types
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Interval (interval)
import Plutus.V1.Ledger.Value (assetClass, assetClassValueOf)
import Plutus.V2.Ledger.Api (TxInfo, scriptContextTxInfo, txInInfoResolved, txInfoInputs, txInfoMint, txInfoOutputs, txInfoValidRange, txOutAddress)
import Plutus.V2.Ledger.Contexts (ScriptContext, ownHash)

{-# INLINEABLE mkStandingBidValidator #-}
mkStandingBidValidator ::
  AuctionTerms ->
  StandingBidDatum ->
  StandingBidRedeemer ->
  ScriptContext ->
  Bool
mkStandingBidValidator terms datum redeemer context =
  let ins = txInfoInputs info
   in length ins == 2
  where
    info :: TxInfo
    info = scriptContextTxInfo context

-- {-# INLINEABLE mkStandingBidValidator #-}
-- mkStandingBidValidator :: AuctionTerms -> StandingBidDatum -> StandingBidRedeemer -> ScriptContext -> Bool
-- mkStandingBidValidator terms datum redeemer context =
--   validAuctionTerms terms
--     && case txInInfoResolved <$> txInfoInputs info of -- All cases require single input
--       [inputOut] -> case redeemer of
--         MoveToHydra ->
--           -- FIXME: new requirements may appear in tech spec
--           -- XXX: using strange check, cuz == for lists failed compilation of Plutus Tx
--           length (txInfoOutputs info) == 1 -- Check that nothing changed in output
--             && head (txInfoOutputs info) == inputOut
--         NewBid -> True
--         -- ( case txInfoOutputs info of
--         --     [out] ->
--         --       -- FIXME: Check bidder has right to make a bid
--         --       traceIfFalse "Output is not into standing bid" $
--         --         txOutAddress out == scriptHashAddress (ownHash context)
--         --           && case validNewBid <$> decodeOutputDatum info inputOut <*> decodeOutputDatum info out of
--         --             Just x -> traceIfFalse "Incorrect bid" x
--         --             Nothing -> traceError "Incorrect encoding for input or output datum"
--         --     _ -> traceError "Not exactly one ouput"
--         -- )
--         --   && interval 0 (biddingEnd terms) == txInfoValidRange info
--         Cleanup ->
--           -- XXX: interval is checked on burning
--           traceIfFalse "Not exactly one voucher was burt during transaction" $
--             let cs = unVoucherCS $ standingBidVoucherCS datum
--                 voucherAC = assetClass cs (stateTokenKindToTokenName Voucher)
--              in assetClassValueOf (txInfoMint info) voucherAC == -1
--                   && ( case txInfoOutputs info of
--                         [out] ->
--                           traceIfFalse
--                             "Output is not to seller"
--                             (txOutAddress out == pubKeyHashAddress (seller terms))
--                             && traceIfFalse
--                               "Output value not min ADA"
--                               ( lovelaceOfOutput out == minAuctionFee
--                               )
--                         _ -> traceError "Not exactly one ouput"
--                      )
--       _ : _ -> traceError "More than one input"
--       [] -> traceError "Impossible happened: no inputs for staning bid validator"
--  where
--   info :: TxInfo
--   info = scriptContextTxInfo context
--   validNewBid :: StandingBidState -> StandingBidState -> Bool
--   validNewBid oldBid (Bid newBidTerms) =
--     case oldBid of
--       Bid oldBidTerms ->
--         traceIfFalse "Bid increment is not greater than minimumBidIncrement" $
--           bidAmount oldBidTerms + minimumBidIncrement terms <= bidAmount newBidTerms
--       NoBid ->
--         traceIfFalse "Bid is not greater than startingBid" $
--           startingBid terms <= bidAmount newBidTerms
--   validNewBid _ NoBid = False
