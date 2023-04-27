{-# OPTIONS_GHC -fno-specialise #-}

module HydraAuction.OnChain.FeeEscrow (mkFeeEscrowValidator) where

-- Prelude imports
import PlutusTx.Prelude
import Prelude (maximum, minimum)

-- Plutus imports

import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api (ScriptContext, TxInfo (..), TxOut (..), Value, adaSymbol, adaToken, scriptContextTxInfo, txInInfoResolved)

-- Hydra auction imports
import HydraAuction.Types (
  AuctionTerms (..),
  FeeEscrowDatum,
  FeeEscrowRedeemer (..),
  )
import HydraAuctionUtils.Types.Natural (naturalToInt)
import HydraAuctionUtils.Plutus (
  nothingForged,
 )

{-# INLINEABLE mkFeeEscrowValidator #-}
mkFeeEscrowValidator :: AuctionTerms -> FeeEscrowDatum -> FeeEscrowRedeemer -> ScriptContext -> Bool
mkFeeEscrowValidator terms () DistributeFees context =
  -- There is one output per delegate. The conditions in validFeeDistribution are satisfied when applied to these outputs and the transaction fee.
  length outputs == length (delegates terms)
    && validFeeDistribution outputs singleFeeInputValue
    -- No tokens are minted or burned.
    && nothingForged info
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    outputs :: [TxOut]
    outputs = txInfoOutputs info

    validFeeDistribution :: [TxOut] -> Value -> Bool
    validFeeDistribution outputsToDelegates txFee =
      allAdaDistributed
        && adaDistributedEvenly
      where
        -- Each delegate received the `auctionFeePerDelegate`,
        -- after deducting the transaction fees from the total.
        allAdaDistributed = actualTotalAda == expectedTotalAda
        actualTotalAda = sum actualAdaValues + adaValueOf txFee
        expectedTotalAda = length (delegates terms) * naturalToInt (auctionFeePerDelegate terms)

        -- The amount received by any delegate differs by at most one lovelace
        -- from what any other delegate received.
        adaDistributedEvenly = True -- 1 > (maximum actualAdaValues - minimum actualAdaValues)
        adaValueOf v = valueOf v adaSymbol adaToken
        actualAdaValues = adaValueOf . txOutValue <$> outputsToDelegates

    -- There is one input spent from the fee escrow validator.
    singleFeeInputValue :: Value
    singleFeeInputValue = case txInInfoResolved <$> txInfoInputs info of
      [] -> traceError "Missing input for fee escrow"
      [feeOut] -> txOutValue feeOut
      _ : _ -> traceError "More than single input from fee escrow validator"
