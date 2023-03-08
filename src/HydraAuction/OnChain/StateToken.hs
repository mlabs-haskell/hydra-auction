{-# OPTIONS_GHC -fno-specialise #-}

module HydraAuction.OnChain.StateToken (StateTokenKind (..), stateTokenKindToTokenName, mkPolicy) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports
import Plutus.V1.Ledger.Interval (contains, from, to)
import Plutus.V1.Ledger.Value (Value, singleton)
import Plutus.V2.Ledger.Api (TokenName (..))
import Plutus.V2.Ledger.Contexts (ScriptContext, TxInfo, ownCurrencySymbol, scriptContextTxInfo, txInInfoOutRef, txInfoInputs, txInfoMint, txInfoOutputs, txInfoValidRange, txOutAddress)

-- Hydra auction imports
import HydraAuction.Addresses (EscrowAddress (..), VoucherCS (..))
import HydraAuction.OnChain.Common (decodeOutputDatum, validAuctionTerms)
import HydraAuction.Types (
  AuctionEscrowDatum (..),
  AuctionState (..),
  AuctionTerms (..),
  VoucherForgingRedeemer (..),
 )

data StateTokenKind = Voucher

{-# INLINEABLE stateTokenKindToTokenName #-}
stateTokenKindToTokenName :: StateTokenKind -> TokenName
stateTokenKindToTokenName Voucher = TokenName "Voucher"

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
