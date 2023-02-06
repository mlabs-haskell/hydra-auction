module HydraAuction.OnChain.Escrow (mkEscrowValidator) where

import PlutusTx.Prelude

import HydraAuction.Addresses
import HydraAuction.OnChain.Common
import HydraAuction.OnChain.StateToken (StateTokenKind (..), stateTokenKindToTokenName)
import HydraAuction.Types
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Interval (contains, from, interval)
import Plutus.V1.Ledger.Value (assetClass, assetClassValueOf)
import Plutus.V2.Ledger.Api (Address, TxInfo, TxOut, scriptContextTxInfo, txInInfoResolved, txInfoInputs, txInfoOutputs, txInfoValidRange, txOutValue)
import Plutus.V2.Ledger.Contexts (ScriptContext, ownHash, txSignedBy)

{-# INLINEABLE mkEscrowValidator #-}
mkEscrowValidator :: (StandingBidAddress, FeeEscrowAddress, AuctionTerms) -> AuctionEscrowDatum -> EscrowRedeemer -> ScriptContext -> Bool
mkEscrowValidator (StandingBidAddress standingBidAddressLocal, FeeEscrowAddress feeEscrowAddressLocal, terms) _ redeemer context =
  traceIfFalse "AuctionTerms is invalid" (validAuctionTerms terms)
    && checkHasSingleEscrowInput
      ( \escrowInputOutput -> case redeemer of
          StartBidding ->
            checkAuctionState (== Announced) escrowInputOutput
              -- TODO
              -- && traceIfFalse "Seller not signed" (txSignedBy info (seller terms))
              -- FIXME
              -- && traceIfFalse "Wrong valid range"
              -- contains (interval (biddingStart terms) (biddingEnd terms)) (txInfoValidRange info)
              && checkStartBiddingOutputs
          SellerReclaims ->
            -- FIXME
            -- contains (from (voucherExpiry terms)) (txInfoValidRange info)
              checkSellerReclaimsOutputs
          BidderBuys ->
            checkAuctionState isStarted escrowInputOutput
              -- FIXME
              -- && contains (interval (biddingEnd terms) (voucherExpiry terms)) (txInfoValidRange info)
              && checkBidderBuys escrowInputOutput
      )
  where
    checkHasSingleEscrowInput cont =
      case escrowInputsOuts of
        [output] -> cont output
        _ : _ -> traceError "More than one escrow input"
        [] -> traceError "Imposible happened: no escrow inputs"
    checkAuctionState statePredicate output =
      case auctionState <$> decodeOutputDatum info output of
        Just x -> traceIfFalse "Wrong auction state" $ statePredicate x
        Nothing -> traceError "Incorrect encoding for auction state datum"
    checkSingleOutput :: Address -> BuiltinString -> (TxOut -> Bool) -> Bool
    checkSingleOutput address outputName cont =
      case byAddress address outputs of
        [output] -> cont output
        _ -> traceError $ "Wrong number of " <> outputName <> " outputs"
    checkSingleOutputWithAmount :: Address -> BuiltinString -> Integer -> Bool
    checkSingleOutputWithAmount address outputName expectedAmount =
      checkSingleOutput
        address
        outputName
        ( \output ->
            traceIfFalse (outputName <> " has wrong amount of ADA") $
              lovelaceOfOutput output == expectedAmount
        )
    checkStartBiddingOutputs =
      case byAddress (scriptHashAddress $ ownHash context) outputs of
        [out] -> traceIfFalse "Auction state is not started" $
          case isStarted <$> (auctionState <$> decodeOutputDatum info out) of
            Just True -> True
            _ -> False
        _ -> traceError "Wrong number of escrow outputs"
        && checkSingleOutput
          standingBidAddressLocal
          "Standing bid"
          ( \out ->
              traceIfFalse "Standing bid does not equalh NoBid" $
                (standingBidState <$> decodeOutputDatum info out) == Just NoBid
          )
    checkBidderBuys escrowInputOutput =
      case byAddress standingBidAddressLocal (txInInfoResolved <$> txInfoInputs info) of
        [standingBidInOut] ->
          -- Check standing bid is authentic (contains state token)
          ( case unVoucherCS <$> (auctionVoucherCS <$> decodeOutputDatum info escrowInputOutput) of
              Just cs ->
                traceIfFalse
                  "Standing bid not contain voucher token"
                  ( let voucherAC = assetClass cs (stateTokenKindToTokenName Voucher)
                     in assetClassValueOf (txOutValue standingBidInOut) voucherAC == 1
                  )
              Nothing -> traceError "Incorrect encoding for escrow input datum"
          )
            -- Check bid is present, bidder signed transaction, auction lot sent to bidder and bid/fee is paid
            && case decodeOutputDatum info standingBidInOut of
              Just (StandingBidDatum {standingBidState}) -> case standingBidState of
                NoBid -> traceError "NoBid in standing bid datum"
                (Bid bidTerms) ->
                  -- traceIfFalse "Not signed by bidder" (txSignedBy info (bidBidder bidTerms))
                    -- checkSingleOutput
                    --   (pubKeyHashAddress $ bidBidder bidTerms)
                    --   "Bidder"
                    --   ( \bidderOutput ->
                    --       traceIfFalse
                    --         "Auction lot not provided to bidder"
                    --         (assetClassValueOf (txOutValue bidderOutput) (auctionLot terms) == 1)
                    --   )
                    checkSingleOutputWithAmount
                      (pubKeyHashAddress $ seller terms)
                      "Seller"
                      (naturalToInt (bidAmount bidTerms) - naturalToInt (auctionFee terms))
                    && checkSingleOutputWithAmount
                      feeEscrowAddressLocal
                      "Fee escrow"
                      (naturalToInt $ auctionFee terms)
              Nothing -> traceError "Incorrect encoding for standing bid datum"
        _ -> traceError "Wrong number of standing bid inputs"
    checkSellerReclaimsOutputs =
      checkSingleOutput
        sellerAddress
        "Seller"
        ( \out ->
            traceIfFalse
              "Auction lot not provided to seller"
              (assetClassValueOf (txOutValue out) (auctionLot terms) == 1)
        )
        && checkSingleOutputWithAmount
          feeEscrowAddressLocal
          "Fee escrow"
          (naturalToInt $ auctionFee terms)
    info :: TxInfo
    info = scriptContextTxInfo context
    outputs = txInfoOutputs info
    sellerAddress = pubKeyHashAddress $ seller terms
    escrowInputsOuts :: [TxOut]
    escrowInputsOuts = byAddress (scriptHashAddress $ ownHash context) $ txInInfoResolved <$> txInfoInputs info
