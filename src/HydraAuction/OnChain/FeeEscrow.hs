{-# OPTIONS_GHC -fno-specialise #-}

module HydraAuction.OnChain.FeeEscrow (mkFeeEscrowValidator) where

-- Prelude imports
import PlutusTx.Prelude

-- import Prelude (quot)

-- Plutus imports
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api (TxInfo (..), TxOut (..), Value, adaSymbol, adaToken, scriptContextTxInfo, txInInfoResolved)
import Plutus.V2.Ledger.Contexts (ScriptContext, ownHash)

-- Hydra auction imports
import HydraAuction.Types (
  AuctionTerms (..),
  FeeEscrowDatum,
  FeeEscrowRedeemer (..),
 )
import HydraAuctionUtils.Plutus (
  byAddress,
  nothingForged,
 )
import HydraAuctionUtils.Types.Natural (naturalToInt)

{-# INLINEABLE mkFeeEscrowValidator #-}
mkFeeEscrowValidator :: AuctionTerms -> FeeEscrowDatum -> FeeEscrowRedeemer -> ScriptContext -> Bool
mkFeeEscrowValidator terms () DistributeFees context =
  -- There is one input spent from the fee escrow validator with enough ADA to pay the fees
  adaValueOf singleFeeInputValue >= expectedFeePerDelegeate * length (delegates terms)
    -- Every delegate is payed at least the expected proportion of fee
    && all receivesProportionOfFee (delegates terms)
    -- No tokens are minted or burned.
    && nothingForged info
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    outputs :: [TxOut]
    outputs = txInfoOutputs info

    adaValueOf v = valueOf v adaSymbol adaToken

    expectedFeePerDelegeate = naturalToInt $ auctionFeePerDelegate terms

    receivesProportionOfFee delegatePKH = case byAddress (pubKeyHashAddress delegatePKH) outputs of
      [] -> traceError "Delegate does not receive proportion of fee"
      outsToDelegate ->
        traceIfFalse "Delegate does not receive proportion of fee" $
          any (\txOut -> adaValueOf (txOutValue txOut) >= naturalToInt (auctionFeePerDelegate terms)) outsToDelegate

    singleFeeInputValue :: Value
    singleFeeInputValue = case byAddress (scriptHashAddress $ ownHash context) $ txInInfoResolved <$> txInfoInputs info of
      [] -> traceError "Missing input for fee escrow"
      [feeOut] -> txOutValue feeOut
      _ : _ -> traceError "More than single input from fee escrow validator"
