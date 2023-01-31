module HydraAuction.OnChain.StateToken (StateTokenKind (..), stateTokenKindToTokenName, mkPolicy) where

import PlutusTx.Prelude

import HydraAuction.Addresses
import HydraAuction.OnChain.Common
import HydraAuction.Types
import Plutus.V1.Ledger.Interval (contains, from)
import Plutus.V1.Ledger.Value (assetClassValueOf, flattenValue)
import Plutus.V2.Ledger.Api (TokenName (..))
import Plutus.V2.Ledger.Contexts (ScriptContext, TxInfo, ownCurrencySymbol, scriptContextTxInfo, txInInfoOutRef, txInInfoResolved, txInfoInputs, txInfoMint, txInfoOutputs, txInfoValidRange, txOutAddress, txOutValue)
import PlutusTx.AssocMap qualified as Map

data StateTokenKind = Voucher

{-# INLINEABLE stateTokenKindToTokenName #-}
stateTokenKindToTokenName :: StateTokenKind -> TokenName
stateTokenKindToTokenName Voucher = TokenName "Voucher"

{-# INLINEABLE mkPolicy #-}
mkPolicy :: (EscrowAddress, AuctionTerms) -> () -> ScriptContext -> Bool
mkPolicy (EscrowAddress escrowAddressLocal, terms) () ctx =
  traceIfFalse "AuctionTerms is invalid" (validAuctionTerms terms)
    && ( case onlyVoucherForgedCount of
          Just x ->
            -- XXX: Pattern matching by integer does not seem to work in Plutus
            case (x == 1, x == -1) of
              (True, False) -> exactlyUtxoRefConsumed && exactlyOneOutputToEscrow
              (False, True) ->
                traceIfFalse "Not exactly one input" (length (txInfoInputs info) == 1)
                  && traceIfFalse "Not exactly none outputs" (length (txInfoInputs info) == 0)
                  && traceIfFalse
                    "Valid range not after voucher expiry"
                    (contains (from (voucherExpiry terms)) (txInfoValidRange info))
              (_, _) -> traceError "Wrong voucher amount forged"
          Nothing -> traceError "Wrong token kind forged"
       )
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    ourTokensForged =
      Map.fromList
        [ (tn, amount)
        | (cs, tn, amount) <- flattenValue (txInfoMint info)
        , ownCurrencySymbol ctx == cs
        ]
    onlyVoucherForgedCount :: Maybe Integer
    onlyVoucherForgedCount =
      case Map.keys ourTokensForged of
        [_] -> Map.lookup tn ourTokensForged
        _ -> Nothing
      where
        tn = stateTokenKindToTokenName Voucher
    exactlyUtxoRefConsumed :: Bool
    exactlyUtxoRefConsumed = case txInfoInputs info of
      [out] ->
        traceIfFalse "Input is not equal to utxoRef" (txInInfoOutRef out == utxoRef terms)
          && traceIfFalse
            "Input does not contain auction lot"
            (assetClassValueOf (txOutValue $ txInInfoResolved out) (auctionLot terms) == 1)
      _ -> traceError "Inputs are not exactly single input"
    expectedOutput :: AuctionEscrowDatum
    expectedOutput = AuctionEscrowDatum Announced (VoucherCS $ ownCurrencySymbol ctx)
    exactlyOneOutputToEscrow :: Bool
    exactlyOneOutputToEscrow = case txInfoOutputs info of
      [output] ->
        traceIfFalse
          "Wrong data in escrow output"
          (decodeOutputDatum info output == Just expectedOutput)
          && traceIfFalse
            "Output not going to escrow contract"
            (txOutAddress output == escrowAddressLocal)
      _ -> traceError "Outputs are not exactly one"
