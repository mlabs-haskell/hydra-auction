module HydraAuction.OnChain.StateToken (StateTokenKind (..), stateTokenKindToTokenName, mkPolicy) where

import PlutusTx.Prelude hiding (elem)

import HydraAuction.Addresses
import HydraAuction.OnChain.Common
import HydraAuction.Types
import Plutus.V1.Ledger.Interval (contains, from)
import Plutus.V1.Ledger.Value (flattenValue)
import Plutus.V2.Ledger.Api (TokenName (..))
import Plutus.V2.Ledger.Contexts (ScriptContext, TxInfo, ownCurrencySymbol, scriptContextTxInfo, txInInfoOutRef, txInfoInputs, txInfoMint, txInfoOutputs, txInfoValidRange, txOutAddress)
import PlutusTx.AssocMap qualified as Map

data StateTokenKind = Voucher

{-# INLINEABLE stateTokenKindToTokenName #-}
stateTokenKindToTokenName :: StateTokenKind -> TokenName
stateTokenKindToTokenName Voucher = TokenName "Voucher"

{-# INLINEABLE elem #-}
elem y (x : xs)
  | x == y = True
  | otherwise = elem y xs
elem _ [] = False

{-# INLINEABLE mkPolicy #-}
mkPolicy :: (EscrowAddress, AuctionTerms) -> () -> ScriptContext -> Bool
mkPolicy (EscrowAddress escrowAddressLocal, terms) () ctx =
  traceIfFalse "AuctionTerms is invalid" (validAuctionTerms terms)
    && ( case onlyVoucherForgedCount of
          Just x ->
            -- XXX: Pattern matching by integer does not seem to work in Plutus
            case (x == 1, x == -1) of
              (True, False) ->
                utxoNonceConsumed && exactlyOneOutputToEscrow
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
    utxoNonceConsumed :: Bool
    utxoNonceConsumed =
      traceIfFalse "Utxo nonce was not consumed at auction announcement." $
        utxoRef terms `elem` (txInInfoOutRef <$> txInfoInputs info)
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
