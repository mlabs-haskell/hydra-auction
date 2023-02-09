module HydraAuction.OnChain.StateToken (StateTokenKind (..), stateTokenKindToTokenName, mkPolicy) where

import PlutusTx.Prelude hiding (elem)

import HydraAuction.Addresses
import HydraAuction.OnChain.Common
import HydraAuction.Types
import Plutus.V1.Ledger.Interval (contains, from, to)
import Plutus.V1.Ledger.Value (Value, singleton)
import Plutus.V2.Ledger.Api (TokenName (..), TxOutRef)
import Plutus.V2.Ledger.Contexts (ScriptContext, TxInfo, ownCurrencySymbol, scriptContextTxInfo, txInInfoOutRef, txInfoInputs, txInfoMint, txInfoOutputs, txInfoValidRange, txOutAddress)

data StateTokenKind = Voucher

{-# INLINEABLE stateTokenKindToTokenName #-}
stateTokenKindToTokenName :: StateTokenKind -> TokenName
stateTokenKindToTokenName Voucher = TokenName "Voucher"

{-# INLINEABLE elem #-}
-- XXX: PlutuxTx fails if we have `Eq a` constraint
elem :: TxOutRef -> [TxOutRef] -> Bool
elem y (x : xs)
  | x == y = True
  | otherwise = elem y xs
elem _ [] = False

{-# INLINEABLE mkPolicy #-}
mkPolicy :: (EscrowAddress, AuctionTerms) -> VoucherForgingRedeemer -> ScriptContext -> Bool
mkPolicy (EscrowAddress escrowAddressLocal, terms) redeemer ctx =
  case redeemer of
    MintVoucher ->
      traceIfFalse "AuctionTerms is invalid" (validAuctionTerms terms)
        && traceIfFalse
          "Valid range not before bidding start"
          (contains (to (biddingStart terms)) (txInfoValidRange info))
        && traceIfFalse "Not exactly one Voucher minted" (txInfoMint info == voucherOnlyValue 1)
        && utxoNonceConsumed
        && exactlyOneOutputToEscrow
    BurnVoucher ->
      traceIfFalse "Not exactly one Voucher burned" (txInfoMint info == voucherOnlyValue (-1))
        && traceIfFalse "Not exactly one input" (length (txInfoInputs info) == 1)
        && traceIfFalse "Not exactly none outputs" (length (txInfoInputs info) == 0)
        && traceIfFalse
          "Valid range not after voucher expiry"
          (contains (from (voucherExpiry terms)) (txInfoValidRange info))
  where
    voucherOnlyValue :: Integer -> Value
    voucherOnlyValue = singleton (ownCurrencySymbol ctx) (stateTokenKindToTokenName Voucher)
    info :: TxInfo
    info = scriptContextTxInfo ctx
    utxoNonceConsumed :: Bool
    utxoNonceConsumed =
      traceIfFalse "Utxo nonce was not consumed at auction announcement." $
        utxoNonce terms `elem` (txInInfoOutRef <$> txInfoInputs info)
    expectedOutput :: AuctionEscrowDatum
    expectedOutput = AuctionEscrowDatum Announced (VoucherCS $ ownCurrencySymbol ctx)
    exactlyOneOutputToEscrow :: Bool
    exactlyOneOutputToEscrow =
      case filter (\x -> escrowAddressLocal == txOutAddress x) $ txInfoOutputs info of
        [output] ->
          case decodeOutputDatum info output of
            Just x ->
              traceIfFalse "Wrong state in escrow output" $
                (auctionState x == auctionState expectedOutput)
                  && traceIfFalse "Wrong auctionVoucherCS in escrow output" (auctionVoucherCS x == auctionVoucherCS expectedOutput)
            Nothing -> traceError "Cannot decode escrow output"
        _ ->
          traceIfFalse "More than one output sent to escrow address" False
