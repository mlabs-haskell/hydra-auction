{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:context-level=2 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize=False #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:pedantic #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:typecheck #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:vervosity=2 #-}

module HydraAuction.OnChain (policy, voucherCurrencySymbol, escrowValidator, escrowAddress, standingBidAddress) where

import PlutusTx.Prelude

import HydraAuction.Addresses
import HydraAuction.OnChain.Common
import HydraAuction.OnChain.Escrow
import HydraAuction.OnChain.StandingBid
import HydraAuction.OnChain.StateToken hiding (mkPolicy)
import HydraAuction.PlutusExtras
import HydraAuction.Types
import Plutus.V2.Ledger.Api (Address, BuiltinData(..), CurrencySymbol, Data(..), MintingPolicy, ScriptContext, ToData (toBuiltinData), TokenName (..), TxInfo, UnsafeFromData (unsafeFromBuiltinData), Validator, mkMintingPolicyScript, mkValidatorScript, txInfoOutputs, txOutAddress)
import Plutus.V2.Ledger.Contexts (ScriptContext, ScriptPurpose(..), TxInfo, ownCurrencySymbol, scriptContextTxInfo, txInInfoOutRef, txInInfoResolved, txInfoInputs, txInfoMint, txInfoOutputs, txInfoValidRange, txOutAddress, txOutValue, scriptContextPurpose)
import Plutus.V1.Ledger.Interval (contains, from)
import Plutus.V1.Ledger.Value (assetClassValueOf, flattenValue)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map

-- State Tokens

{-# INLINEABLE myCheck #-}
myCheck True = ()
myCheck False = traceError "Check error"

{-# INLINEABLE mkPolicy #-}
mkPolicy :: (EscrowAddress, AuctionTerms) -> () -> ScriptContext -> Bool
mkPolicy (EscrowAddress escrowAddressLocal, terms) () ctx =
  -- traceIfFalse "AuctionTerms is invalid" (validAuctionTerms terms)
  True
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
              (_, _) -> traceIfFalse "Wrong voucher amount forged" False
          Nothing -> traceIfFalse "Wrong token kind forged" False
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
      Just 1
    -- case [Map.keys ourTokensForged] of
    --   [_] -> Map.lookup tn ourTokensForged
    --   _ -> Nothing
    -- where
    --   tn = stateTokenKindToTokenName Voucher
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
          traceIfFalse "Wrong data in escrow output" True
            -- (decodeOutputDatum info output == Just expectedOutput)
        _ ->
          traceIfFalse "More than one output sent to escrow address" False

policy :: AuctionTerms -> MintingPolicy
policy !terms =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\x y z -> myCheck (mkPolicy x (unsafeFromBuiltinData y) (unsafeFromBuiltinData z))||])
      `PlutusTx.applyCode` PlutusTx.liftCode (escrowAddress terms, terms)

voucherCurrencySymbol :: AuctionTerms -> CurrencySymbol
voucherCurrencySymbol = scriptCurrencySymbol . policy

-- Escrow contract

{-# INLINEABLE escrowValidator #-}
escrowValidator :: AuctionTerms -> Validator
escrowValidator terms =
  mkValidatorScript $
    $$(PlutusTx.compile [||\_ _ _ _ -> ()||])
      `PlutusTx.applyCode` PlutusTx.liftCode (standingBidAddress terms, feeEscrowAddress terms, terms)

{-# INLINEABLE escrowAddress #-}
escrowAddress :: AuctionTerms -> EscrowAddress
escrowAddress = EscrowAddress . validatorAddress . escrowValidator

-- Standing bid contract

{-# INLINEABLE standingBidValidator #-}
standingBidValidator :: AuctionTerms -> Validator
standingBidValidator terms =
  mkValidatorScript $
    $$(PlutusTx.compile [||\_ _ _ _ -> ()||])
      `PlutusTx.applyCode` PlutusTx.liftCode terms

{-# INLINEABLE standingBidAddress #-}
standingBidAddress :: AuctionTerms -> StandingBidAddress
standingBidAddress = StandingBidAddress . validatorAddress . standingBidValidator

-- Fee escrow

mkFeeEscrowValidator :: AuctionTerms -> () -> () -> ScriptContext -> Bool
mkFeeEscrowValidator _terms _datum () _context = True -- FIXUP: Implement

{-# INLINEABLE feeEscrowValidator #-}
feeEscrowValidator :: AuctionTerms -> Validator
feeEscrowValidator terms =
  mkValidatorScript $
    $$(PlutusTx.compile [||wrapValidator . mkFeeEscrowValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode terms

{-# INLINEABLE feeEscrowAddress #-}
feeEscrowAddress :: AuctionTerms -> FeeEscrowAddress
feeEscrowAddress = FeeEscrowAddress . validatorAddress . standingBidValidator
