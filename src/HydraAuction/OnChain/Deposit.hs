{-# OPTIONS_GHC -fno-specialise #-}

module HydraAuction.OnChain.Deposit (mkDepositValidator) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports

import Plutus.V1.Ledger.Address (Address, pubKeyHashAddress)
import Plutus.V1.Ledger.Interval (contains, from)
import Plutus.V2.Ledger.Api (TxInfo, scriptContextTxInfo, txInInfoResolved, txInfoOutputs, txInfoReferenceInputs, txInfoValidRange, txOutAddress)
import Plutus.V2.Ledger.Contexts (ScriptContext, txSignedBy)
import PlutusTx qualified

-- Hydra auction imports
import HydraAuction.Addresses (EscrowAddress (..), StandingBidAddress (..))
import HydraAuction.OnChain.Common (
  byAddress,
  decodeOutputDatum,
  nothingForged,
 )
import HydraAuction.Types (
  AuctionEscrowDatum (..),
  AuctionTerms (..),
  BidDepositDatum (..),
  BidDepositRedeemer (..),
  BidTerms (..),
  StandingBidDatum (..),
  StandingBidState (..),
 )

{-# INLINEABLE mkDepositValidator #-}
mkDepositValidator :: (StandingBidAddress, EscrowAddress, AuctionTerms) -> BidDepositDatum -> BidDepositRedeemer -> ScriptContext -> Bool
mkDepositValidator (StandingBidAddress standingBidAddr, EscrowAddress escrowAddr, terms) datum redeemer context = case redeemer of
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
        "Wrong interval for Losing Bidder"
        (contains (from (biddingEnd terms)) (txInfoValidRange info))
      -- No tokens are minted or burned
      && nothingForged info
  SellerClaimsDeposit ->
    -- There is one reference input from the auction escrow validator, mentioning the same voucher and voucher expiry time.
    traceIfFalse "Voucher CS does not match auction escrow" (auctionVoucherCS auctionEscrowDatum == bidDepositVoucherCS datum)
      -- There is one reference input from the standing bid validator, mentioning the same voucher and bidder as the bid deposit input.
      && traceIfFalse "Voucher CS does not match standing bid" (standingBidVoucherCS standingBidInputDatum == bidDepositVoucherCS datum)
      && traceIfFalse "Bidder deposit input does not match standing bid validator" ((bidBidder <$> standingBid (standingBidState standingBidInputDatum)) == Just (bidDepositBidder datum))
      -- The transaction validity interval starts after the voucher expiry time.
      && traceIfFalse
        "Wrong interval for Losing"
        (contains (from (voucherExpiry terms)) (txInfoValidRange info))
      -- The transaction is signed by the seller.
      && traceIfFalse "Seller not signed" (txSignedBy info (seller terms))
      -- No tokens are minted or burned
      && nothingForged info
  WinningBidder -> False
  CleanupDeposit -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    decodeReferenceInputDatum :: PlutusTx.FromData a => Address -> BuiltinString -> a
    decodeReferenceInputDatum addr name = case byAddress addr $ txInInfoResolved <$> txInfoReferenceInputs info of
      [] -> traceError $ "Missing reference input for" <> name
      [refIn] -> case decodeOutputDatum info refIn of
        Just d -> d
        Nothing -> traceError $ "Can not decode " <> name <> " input datum"
      _ : _ -> traceError $ "More than single reference input from " <> name <> " validator"

    auctionEscrowDatum :: AuctionEscrowDatum
    auctionEscrowDatum = decodeReferenceInputDatum escrowAddr "auction escrow"

    standingBidInputDatum :: StandingBidDatum
    standingBidInputDatum = decodeReferenceInputDatum standingBidAddr "standing bid"

    bidDepositToKey pkh = case txInfoOutputs info of
      [out] ->
        traceIfFalse
          "Output sent to incorrect address"
          (txOutAddress out == pubKeyHashAddress pkh)
      _ -> traceError "Incorrect number of outputs"
