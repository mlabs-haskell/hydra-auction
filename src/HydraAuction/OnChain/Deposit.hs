{-# OPTIONS_GHC -fno-specialise #-}

module HydraAuction.OnChain.Deposit (mkDepositValidator) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports

import Plutus.V1.Ledger.Address (Address, pubKeyHashAddress)
import Plutus.V1.Ledger.Interval (contains, from)
import Plutus.V2.Ledger.Api (TxInfo, scriptContextTxInfo, txInInfoResolved, txInfoInputs, txInfoOutputs, txInfoReferenceInputs, txInfoValidRange, txOutAddress)
import Plutus.V2.Ledger.Contexts (ScriptContext, txSignedBy)
import PlutusTx qualified

-- Hydra auction imports
import HydraAuction.Addresses (EscrowAddress (..), StandingBidAddress (..))
import HydraAuction.Types (
  AuctionEscrowDatum (..),
  AuctionTerms (..),
  BidDepositDatum (..),
  BidDepositRedeemer (..),
  BidTerms (..),
  StandingBidDatum (..),
  StandingBidState (..),
 )
import HydraAuctionUtils.Plutus (
  byAddress,
  decodeOutputDatum,
  nothingForged,
 )

{-# INLINEABLE mkDepositValidator #-}
mkDepositValidator :: (StandingBidAddress, EscrowAddress, AuctionTerms) -> BidDepositDatum -> BidDepositRedeemer -> ScriptContext -> Bool
mkDepositValidator (StandingBidAddress standingBidAddr, EscrowAddress escrowAddr, terms) datum redeemer context = case redeemer of
  LosingBidder ->
    -- Signer of this tx matches bidder in deposit datum
    traceIfFalse "Bidder not signed" (txSignedBy info (bidDepositBidder datum))
      -- Single input from standing bid validator with matching vouhcer
      && traceIfFalse "Voucher CS does not match" (standingBidVoucherCS standingBidReferenceInputDatum == bidDepositVoucherCS datum)
      -- One output send to bidder containing deposit
      && checkBidDepositOutToBidder
      -- Standing bid winner is not current bidder
      && traceIfFalse
        "Standing bid winner must not be current bidder"
        ((bidderPKH <$> standingBid (standingBidState standingBidReferenceInputDatum)) /= Just (bidDepositBidder datum))
      -- The transaction validity time starts after the bidding ends
      && traceIfFalse
        "Wrong interval for Losing Bidder"
        (contains (from (biddingEnd terms)) (txInfoValidRange info))
      -- No tokens are minted or burned
      && nothingForged info
  SellerClaimsDeposit ->
    -- There is one reference input from the auction escrow validator, mentioning the same voucher and voucher expiry time.
    traceIfFalse "Voucher CS does not match auction escrow" (auctionVoucherCS auctionEscrowReferenceInputDatum == bidDepositVoucherCS datum)
      -- There is one reference input from the standing bid validator, mentioning the same voucher and bidder as the bid deposit input.
      && traceIfFalse "Voucher CS does not match standing bid" (standingBidVoucherCS standingBidReferenceInputDatum == bidDepositVoucherCS datum)
      && traceIfFalse "Bidder deposit input does not match standing bid validator" ((bidderPKH <$> standingBid (standingBidState standingBidReferenceInputDatum)) == Just (bidDepositBidder datum))
      -- The transaction validity interval starts after the voucher expiry time.
      && traceIfFalse
        "Wrong interval for Losing"
        (contains (from (voucherExpiry terms)) (txInfoValidRange info))
      -- The transaction is signed by the seller.
      && traceIfFalse "Seller not signed" (txSignedBy info (sellerPKH terms))
      -- No tokens are minted or burned
      && nothingForged info
  WinningBidder ->
    -- There is one input spent from the auction escrow validator, mentioning the same voucher as the bid deposit in its datum.
    traceIfFalse "Voucher CS does not match auction escrow" (auctionVoucherCS auctionEscrowInputDatum == bidDepositVoucherCS datum)
      -- The transaction is signed by the bidder.
      && traceIfFalse "Bidder not signed" (txSignedBy info (bidDepositBidder datum))
      -- No tokens are minted or burned.
      && nothingForged info
  CleanupDeposit ->
    -- There is one output sent to the bidder containing the bid deposit.
    checkBidDepositOutToBidder
      -- The transaction validity time starts after the cleanup time.
      && traceIfFalse
        "Wrong interval for Losing"
        (contains (from (cleanup terms)) (txInfoValidRange info))
      -- No tokens are minted or burned.
      && nothingForged info
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    decodeReferenceInputDatum :: PlutusTx.FromData a => Address -> BuiltinString -> a
    decodeReferenceInputDatum addr name = case byAddress addr $ txInInfoResolved <$> txInfoReferenceInputs info of
      [] -> traceError $ "Missing reference input for " <> name
      [refIn] -> case decodeOutputDatum info refIn of
        Just d -> d
        Nothing -> traceError $ "Can not decode " <> name <> " input datum"
      _ : _ -> traceError $ "More than single reference input from " <> name <> " validator"

    auctionEscrowReferenceInputDatum :: AuctionEscrowDatum
    auctionEscrowReferenceInputDatum = decodeReferenceInputDatum escrowAddr "auction escrow"

    standingBidReferenceInputDatum :: StandingBidDatum
    standingBidReferenceInputDatum = decodeReferenceInputDatum standingBidAddr "standing bid"

    auctionEscrowInputDatum :: AuctionEscrowDatum
    auctionEscrowInputDatum = case byAddress escrowAddr $ txInInfoResolved <$> txInfoInputs info of
      [] -> traceError "Missing input for auction escrow"
      [auctionEscrowIn] -> case decodeOutputDatum info auctionEscrowIn of
        Just d -> d
        Nothing -> traceError "Can not decode auction escrow input datum"
      _ : _ -> traceError "More than single input from auction escrow validator"

    checkBidDepositOutToBidder = case txInfoOutputs info of
      [out] ->
        traceIfFalse
          "Output sent to incorrect address"
          (txOutAddress out == pubKeyHashAddress (bidDepositBidder datum))
      _ -> traceError "Incorrect number of outputs"
