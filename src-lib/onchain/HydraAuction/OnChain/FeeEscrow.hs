module HydraAuction.OnChain.FeeEscrow (mkFeeEscrowValidator) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V2 (TxInfo (..), TxOut (..), Value, adaSymbol, adaToken, scriptContextTxInfo, txInInfoResolved)
import PlutusLedgerApi.V2.Contexts (ScriptContext, findOwnInput)

-- Hydra auction imports
import HydraAuction.OnChain.Common (checkNonAdaOutputsNum)
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
    && checkNonAdaOutputsNum context 0
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    ownAddress = case findOwnInput context of
      Just x -> txOutAddress $ txInInfoResolved x
      Nothing -> traceError "Impossible happened"

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
    singleFeeInputValue = case byAddress ownAddress $ txInInfoResolved <$> txInfoInputs info of
      [] -> traceError "Missing input for fee escrow"
      [feeOut] -> txOutValue feeOut
      _ : _ -> traceError "More than single input from fee escrow validator"