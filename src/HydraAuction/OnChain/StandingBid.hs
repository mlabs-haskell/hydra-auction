module HydraAuction.OnChain.StandingBid (mkStandingBidValidator) where

import PlutusTx.Prelude

import HydraAuction.Addresses
import HydraAuction.OnChain.Common
import HydraAuction.OnChain.StateToken (StateTokenKind (..), stateTokenKindToTokenName)
import HydraAuction.Types
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Interval (interval)
import Plutus.V1.Ledger.Value (assetClass, assetClassValueOf)
import Plutus.V2.Ledger.Api (
  Datum,
  OutputDatum (OutputDatum),
  TxInfo,
  TxOut,
  fromBuiltinData,
  getDatum,
  scriptContextTxInfo,
  txInInfoResolved,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoValidRange,
  txOutAddress,
 )
import Plutus.V2.Ledger.Contexts (
  ScriptContext,
  getContinuingOutputs,
  ownHash,
  txOutDatum,
 )

{-# INLINEABLE mkStandingBidValidator #-}
mkStandingBidValidator ::
  AuctionTerms ->
  StandingBidDatum ->
  StandingBidRedeemer ->
  ScriptContext ->
  Bool
mkStandingBidValidator terms datum redeemer context =
  validAuctionTerms terms
    && case getScriptInputs info context of
      [inputOut] -> case redeemer of
        MoveToHydra ->
          -- FIXME: new requirements may appear in tech spec
          -- XXX: using strange check, cuz == for lists failed compilation of Plutus Tx
          length (txInfoOutputs info) == 1 -- Check that nothing changed in output
            && head (txInfoOutputs info) == inputOut
        NewBid ->
          ( case getContinuingOutputs context of
              [out] ->
                case txOutDatum out of
                  OutputDatum dat ->
                    let datum' = parseStandingBidDatum dat
                     in validNewBid (standingBidState datum) (standingBidState datum')
                  _ -> traceError "Missing datum"
              _ -> traceError "Not exactly one ouput"
          )
        -- FIXME: Reinstate
        -- && interval 0 (biddingEnd terms) == txInfoValidRange info
        Cleanup ->
          -- XXX: interval is checked on burning
          traceIfFalse "Not exactly one voucher was burt during transaction" $
            let cs = unVoucherCS $ standingBidVoucherCS datum
                voucherAC = assetClass cs (stateTokenKindToTokenName Voucher)
             in assetClassValueOf (txInfoMint info) voucherAC == -1
                  && ( case txInfoOutputs info of
                        [out] ->
                          traceIfFalse
                            "Output is not to seller"
                            (txOutAddress out == pubKeyHashAddress (seller terms))
                            && traceIfFalse
                              "Output value not min ADA"
                              ( lovelaceOfOutput out == minAuctionFee
                              )
                        _ -> traceError "Not exactly one ouput"
                     )
      _ -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    parseStandingBidDatum :: Datum -> StandingBidDatum
    parseStandingBidDatum dat =
      case fromBuiltinData (getDatum dat) of
        Nothing -> traceError "Cannot parse standing bid datum"
        Just sbd -> sbd

    validNewBid :: StandingBidState -> StandingBidState -> Bool
    validNewBid oldBid (Bid newBidTerms) =
      case oldBid of
        Bid oldBidTerms ->
          traceIfFalse "Bid increment is not greater than minimumBidIncrement" $
            bidAmount oldBidTerms + minimumBidIncrement terms
              <= bidAmount newBidTerms
        NoBid ->
          traceIfFalse "Bid is not greater than startingBid" $
            startingBid terms <= bidAmount newBidTerms
    validNewBid _ NoBid = False

{-# INLINEABLE getScriptInputs #-}
getScriptInputs :: TxInfo -> ScriptContext -> [TxOut]
getScriptInputs info ctx =
  filter
    (\txIn -> txOutAddress txIn == scriptHashAddress (ownHash ctx))
    (txInInfoResolved <$> txInfoInputs info)
