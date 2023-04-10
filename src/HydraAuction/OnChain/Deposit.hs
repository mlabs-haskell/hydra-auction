{-# OPTIONS_GHC -fno-specialise #-}

module HydraAuction.OnChain.Deposit (mkDepositValidator) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.V1.Ledger.Interval (contains, from)
import Plutus.V2.Ledger.Api (TxInfo, scriptContextTxInfo, txInInfoResolved, txInfoOutputs, txInfoReferenceInputs, txInfoValidRange, txOutAddress)
import Plutus.V2.Ledger.Contexts (ScriptContext, txSignedBy)

-- Hydra auction imports
import HydraAuction.Addresses (StandingBidAddress (..))
import HydraAuction.OnChain.Common (
  byAddress,
  decodeOutputDatum,
  nothingForged,
 )
import HydraAuction.Types (
  AuctionTerms (..),
  BidDepositDatum (..),
  BidDepositRedeemer (..),
  BidTerms (..),
  StandingBidDatum (..),
  StandingBidState (..),
 )

{-# INLINEABLE mkDepositValidator #-}
mkDepositValidator :: (StandingBidAddress, AuctionTerms) -> BidDepositDatum -> BidDepositRedeemer -> ScriptContext -> Bool
mkDepositValidator (StandingBidAddress standingBidAddr, terms) datum redeemer context = case redeemer of
  LosingBidder ->
    -- Signer of this tx matches bidder in deposit datum
    traceIfFalse "Bidder not signed" (txSignedBy info (bidDepositBidder datum))
      -- Single input from standing bid validator with matching vouhcer
      && traceIfFalse "Voucher CS does not match" (standingBidVoucherCS standingBidInputDatum == bidDepositVoucherCS datum)
      -- One output send to bidder containing deposit
      && bidDepositToKey (bidDepositBidder datum)
      -- Standing bid winner is not current bidder
      && traceIfFalse
        "Standing bid winner must not be current bidder"
        ((bidBidder <$> standingBid (standingBidState standingBidInputDatum)) /= Just (bidDepositBidder datum))
      -- Tx validity >= bidding ends
      && traceIfFalse
        "Wrong interval for Losing"
        (contains (from (biddingEnd terms)) (txInfoValidRange info))
      -- Nothing minted
      && nothingForged info
  _ -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    standingBidInputDatum :: StandingBidDatum
    standingBidInputDatum = case byAddress standingBidAddr $ txInInfoResolved <$> txInfoReferenceInputs info of
      [] -> traceError "Missing input for standing bid validator"
      [standingBidIn] -> case decodeOutputDatum info standingBidIn of
        Just d -> d
        Nothing -> traceError "Can not decode standing bid input datum"
      _ : _ -> traceError "More than single input from standing bid validator"

    bidDepositToKey pkh = case txInfoOutputs info of
      [out] ->
        traceIfFalse
          "Output sent to incorrect address"
          (txOutAddress out == pubKeyHashAddress pkh)
      _ -> traceError "Incorrect number of outputs"
