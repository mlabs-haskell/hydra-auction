module HydraAuction.OnChain.Escrow (mkEscrowValidator) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports
import PlutusLedgerApi.V1.Address (Address, pubKeyHashAddress)
import PlutusLedgerApi.V1.Value (assetClass, assetClassValueOf, getValue)
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (..),
  TxInfo (..),
  findOwnInput,
  txInInfoResolved,
  txSignedBy,
 )
import PlutusLedgerApi.V2.Tx (TxOut (..))

-- Hydra auction imports
import HydraAuction.Addresses (
  FeeEscrowAddress (..),
  StandingBidAddress (..),
  VoucherCS (..),
 )
import HydraAuction.OnChain.Common (
  checkInterval,
  checkVoucherExpiredOrLater,
 )
import HydraAuction.OnChain.StateToken (
  StateTokenKind (..),
  stateTokenKindToTokenName,
 )
import HydraAuction.Types (
  AuctionEscrowDatum (..),
  AuctionStage (..),
  AuctionState (..),
  AuctionTerms (..),
  BidTerms (..),
  EscrowRedeemer (..),
  StandingBidDatum (..),
  StandingBidState (..),
  calculateTotalFee,
  isStarted,
 )
import HydraAuctionUtils.Plutus (
  byAddress,
  decodeOutputDatum,
  lovelaceOfOutput,
  nothingForged,
 )
import HydraAuctionUtils.Types.Natural (naturalToInt)

{-# INLINEABLE mkEscrowValidator #-}
mkEscrowValidator :: (StandingBidAddress, FeeEscrowAddress, AuctionTerms) -> AuctionEscrowDatum -> EscrowRedeemer -> ScriptContext -> Bool
mkEscrowValidator (StandingBidAddress standingBidAddressLocal, FeeEscrowAddress feeEscrowAddressLocal, terms) _ redeemer context =
  nothingForged info
    && checkHasSingleEscrowInput
      ( \escrowInputOutput -> case redeemer of
          StartBidding ->
            checkAuctionState (== Announced) escrowInputOutput
              && traceIfFalse "Seller not signed" (txSignedBy info (sellerPKH terms))
              && checkInterval terms BiddingStartedStage info
              && checkStartBiddingOutputs
          SellerReclaims ->
            checkVoucherExpiredOrLater terms info
              && checkSellerReclaimsOutputs
          BidderBuys ->
            checkAuctionState isStarted escrowInputOutput
              && checkInterval terms BiddingEndedStage info
              && checkBidderBuys escrowInputOutput
      )
  where
    ownAddress = case findOwnInput context of
      Just x -> txOutAddress $ txInInfoResolved x
      Nothing -> traceError "Impossible happened"
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
    checkSingleNonAdaOutput :: Address -> BuiltinString -> (TxOut -> Bool) -> Bool
    checkSingleNonAdaOutput address outputName cont =
      case filter isNotAdaOnlyOutput $ byAddress address outputs of
        [output] -> cont output
        _ -> traceError $ "Wrong number of " <> outputName <> "non-ADA-only outputs"
      where
        isNotAdaOnlyOutput output =
          let value = txOutValue output
           in length (getValue value) > 1
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
      case byAddress ownAddress outputs of
        [out] -> traceIfFalse "Auction state is not started" $
          case isStarted <$> (auctionState <$> decodeOutputDatum info out) of
            Just True -> True
            _ -> False
        _ -> traceError "Wrong number of escrow outputs"
        && checkSingleOutput
          standingBidAddressLocal
          "Standing bid"
          ( \out ->
              traceIfFalse "Standing bid should be Nothing" $
                (standingBid <$> (standingBidState <$> decodeOutputDatum info out)) == Just Nothing
          )
    checkBidderBuys escrowInputOutput =
      case byAddress standingBidAddressLocal (txInInfoResolved <$> txInfoReferenceInputs info) of
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
              Just (StandingBidDatum {standingBidState}) -> case standingBid standingBidState of
                Nothing -> traceError "No bid in standing bid datum"
                (Just bidTerms) ->
                  traceIfFalse "Not signed by bidder" (txSignedBy info (bidderPKH bidTerms))
                    && checkSingleNonAdaOutput
                      (pubKeyHashAddress $ bidderPKH bidTerms)
                      "Bidder"
                      ( \bidderOutput ->
                          traceIfFalse
                            "Auction lot not provided to bidder"
                            (assetClassValueOf (txOutValue bidderOutput) (auctionLot terms) == 1)
                      )
                    && checkSingleOutputWithAmount
                      (pubKeyHashAddress $ sellerPKH terms)
                      "Seller"
                      (naturalToInt (bidAmount bidTerms) - totalFeeToPay)
                    && checkSingleOutputWithAmount
                      feeEscrowAddressLocal
                      "Fee escrow"
                      totalFeeToPay
              Nothing -> traceError "Incorrect encoding for standing bid datum"
        _ -> traceError "Wrong number of standing bid inputs"
    checkSellerReclaimsOutputs =
      checkSingleNonAdaOutput
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
          totalFeeToPay
    info :: TxInfo
    info = scriptContextTxInfo context
    outputs = txInfoOutputs info
    sellerAddress = pubKeyHashAddress $ sellerPKH terms
    totalFeeToPay = calculateTotalFee terms
    escrowInputsOuts :: [TxOut]
    escrowInputsOuts = byAddress ownAddress $ txInInfoResolved <$> txInfoInputs info
