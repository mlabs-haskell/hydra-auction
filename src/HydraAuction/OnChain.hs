{-# LANGUAGE RecordWildCards #-}

module HydraAuction.OnChain (mkPolicy, voucherCurrencySymbol, mkEscrowValidator, escrowAddress, standingBidAddress) where

import PlutusTx.Prelude

import HydraAuction.Addresses
import HydraAuction.Types
import Plutus.Extras
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Contexts
import Plutus.V1.Ledger.Interval (contains, interval)
import Plutus.V1.Ledger.Value (assetClass, assetClassValueOf, flattenValue)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map

-- Common

minAuctionFee :: Integer
minAuctionFee = 2

validAuctionTerms' :: AuctionTerms -> POSIXTime -> Bool
validAuctionTerms' AuctionTerms {..} announcementTxValidityUpperBound =
  announcementTxValidityUpperBound < biddingStart
    && biddingStart < biddingEnd
    && biddingEnd < voucherExpiry
    && voucherExpiry < cleanup
    && naturalToInt minimumBidIncrement > 0
    && startingBid > auctionFee
    && naturalToInt auctionFee > length delegates * minAuctionFee
    && not (null delegates)
    && modulo (naturalToInt auctionFee) (length delegates) == 0

-- FIXME: check interval from TxInfo
validAuctionTerms :: AuctionTerms -> Bool
validAuctionTerms terms = validAuctionTerms' terms (POSIXTime 0)

{-# INLINEABLE decodeOutputDatum #-}
decodeOutputDatum :: PlutusTx.FromData a => TxInfo -> TxOut -> Maybe a
decodeOutputDatum info output = do
  hash <- txOutDatumHash output
  datum <- findDatum hash info
  fromBuiltinData $ getDatum datum

{-# INLINEABLE byAddress #-}
byAddress :: Address -> [TxOut] -> [TxOut]
byAddress address = filter (\o -> txOutAddress o == address)

-- FIXME: Use proper types to work with Ada/Lovelace distinction
adaToLovelace :: Integer -> Integer
adaToLovelace = (*) 1_000_000

-- XXX: Plutus.V1.Ledger.Ada module requires more dependencies
lovelaceOfOutput :: TxOut -> Integer
lovelaceOfOutput output = assetClassValueOf (txOutValue output) ac
  where
    ac = assetClass (CurrencySymbol emptyByteString) (TokenName emptyByteString)

-- State Tokens

data StateTokenKind = Voucher

{-# INLINEABLE stateTokenKindToTokenName #-}
stateTokenKindToTokenName :: StateTokenKind -> TokenName
stateTokenKindToTokenName Voucher = TokenName "Voucher"

{-# INLINEABLE mkPolicy #-}
mkPolicy :: (EscrowAddress, AuctionTerms) -> () -> ScriptContext -> Bool
mkPolicy (EscrowAddress escrowAddressLocal, terms) () ctx =
  traceIfFalse "AuctionTerms is invalid" (validAuctionTerms terms)
    && ( case onlyVoucherForgedCount of
          Just x ->
            -- XXX: Pattern matching by integer does not seem to work in Plutus
            case (x == 1, x == -1) of
              (True, False) -> exactlyUtxoRefConsumed && exactlyOneOutputToEscrow
              (False, True) ->
                traceIfFalse "Valid range not after voucher expiry" $
                  contains (from (voucherExpiry terms)) (txInfoValidRange info)
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
    exactlyUtxoRefConsumed :: Bool
    exactlyUtxoRefConsumed = case txInfoInputs info of
      [out] ->
        traceIfFalse "Input is not equal to utxoRef" (txInInfoOutRef out == utxoRef terms)
          && traceIfFalse
            "Input does not contain auction lot"
            (assetClassValueOf (txOutValue $ txInInfoResolved out) (auctionLot terms) == 1)
      _ -> traceError "Inputs are not exactly single input"
    expectedOutput :: AuctionEscrowDatum
    expectedOutput = AuctionEscrowDatum Announced (ownCurrencySymbol ctx)
    exactlyOneOutputToEscrow :: Bool
    exactlyOneOutputToEscrow = case txInfoOutputs info of
      [output] ->
        traceIfFalse
          "Wrong data in escrow output"
          (decodeOutputDatum info output == Just expectedOutput)
          && traceIfFalse
            "Output not going to escrow contract"
            (txOutAddress output == escrowAddressLocal)
      _ -> traceError "Outputs are not exactly one"

policy :: AuctionTerms -> MintingPolicy
policy terms =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode (escrowAddress terms, terms)

voucherCurrencySymbol :: AuctionTerms -> CurrencySymbol
voucherCurrencySymbol = scriptCurrencySymbol . policy

-- Escrow contract

mkEscrowValidator :: (EscrowAddress, StandingBidAddress, FeeEscrowAddress, AuctionTerms) -> AuctionEscrowDatum -> EscrowRedeemer -> ScriptContext -> Bool
mkEscrowValidator (EscrowAddress escrowAddressLocal, StandingBidAddress standingBidAddressLocal, FeeEscrowAddress feeEscrowAddressLocal, terms) _ redeemer context =
  traceIfFalse "AuctionTerms is invalid" (validAuctionTerms terms)
    && checkHasSingleEscrowInput
    $ \escrowInputOutput -> case redeemer of
      StartBidding ->
        checkAuctionState (== Announced) escrowInputOutput
          && txSignedBy info (seller terms)
          && contains (interval (biddingStart terms) (biddingEnd terms)) (txInfoValidRange info)
          && checkStartBiddingOutputs
      SellerReclaims ->
        checkAuctionState isStarted escrowInputOutput
          && contains (from (voucherExpiry terms)) (txInfoValidRange info)
          && checkSellerReclaimsOutputs
      BidderBuys ->
        checkAuctionState isStarted escrowInputOutput
          && contains (interval (biddingEnd terms) (voucherExpiry terms)) (txInfoValidRange info)
          && checkBidderBuys escrowInputOutput
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
    checkStartBiddingOutputs =
      traceError "Not two outputs" $
        length outputs == 2
          && case byAddress escrowAddressLocal outputs of
            [out] -> traceIfFalse "Auction state is not started" $
              case isStarted <$> (auctionState <$> decodeOutputDatum info out) of
                Just True -> True
                _ -> False
            _ -> traceError "Wrong number of escrow outputs"
          && case byAddress standingBidAddressLocal outputs of
            [out] ->
              traceIfFalse "Standing bid does not equalh NoBid" $
                (standingBid <$> decodeOutputDatum info out) == Just NoBid
            _ -> traceError "Wrong number of standing bid outputs"
    checkBidderBuys escrowInputOutput =
      case byAddress standingBidAddressLocal (txInInfoResolved <$> txInfoInputs info) of
        [standingBidInOut] ->
          -- Check standing bid is authentic (contains state token)
          ( case auctionVoucherCS <$> decodeOutputDatum info escrowInputOutput of
              Just cs ->
                traceIfFalse
                  "Standing bid not contain voucher token"
                  ( let voucherAC = assetClass cs (stateTokenKindToTokenName Voucher)
                     in assetClassValueOf (txOutValue standingBidInOut) voucherAC == 1
                  )
              Nothing -> traceError "Incorrect encoding for escrow input datum"
          )
            -- Check bid is present, auction lot sent to bidder and bid/fee is paid
            && case decodeOutputDatum info standingBidInOut of
              Just NoBid -> traceError "No bid in standing bid datum"
              Just (Bid bidTerms) ->
                ( case byAddress (pubKeyHashAddress $ bidBidder bidTerms) outputs of
                    [bidderOutput] ->
                      traceIfFalse
                        "Auction lot not provided to bidder"
                        (assetClassValueOf (txOutValue bidderOutput) (auctionLot terms) == 1)
                    _ -> traceError "Wrong number of bidder outputs"
                )
                  && ( case byAddress (pubKeyHashAddress $ seller terms) outputs of
                        [sellerOut] ->
                          traceIfFalse "Seller output has wrong amount of ADA" $
                            lovelaceOfOutput sellerOut
                              == (naturalToInt (bidAmount bidTerms) - adaToLovelace (naturalToInt $ auctionFee terms))
                        _ -> traceError "Wrong number of seller outputs"
                     )
                  && ( case byAddress feeEscrowAddressLocal outputs of
                        [out] ->
                          traceIfFalse "Fee output has wrong amount of ADA" $
                            lovelaceOfOutput out == adaToLovelace (naturalToInt $ auctionFee terms)
                        _ -> traceError "Wrong number of fee escrow outputs"
                     )
              Nothing -> traceError "Incorrect encoding for standing bid datum"
        _ -> traceError "Wrong number of standing bid inputs"
    checkSellerReclaimsOutputs =
      ( case byAddress sellerAddress outputs of
          [out] ->
            traceIfFalse
              "Auction lot not provided to seller"
              (assetClassValueOf (txOutValue out) (auctionLot terms) == 1)
          _ -> traceError "Wrong number of seller outputs"
      )
        && ( case byAddress feeEscrowAddressLocal outputs of
              [out] ->
                traceIfFalse "Fee output has wrong amount of ADA" $
                  lovelaceOfOutput out == adaToLovelace (naturalToInt $ auctionFee terms)
              _ -> traceError "Wrong number of fee escrow outputs"
           )
    info :: TxInfo
    info = scriptContextTxInfo context
    outputs = txInfoOutputs info
    sellerAddress = pubKeyHashAddress $ seller terms
    escrowInputsOuts :: [TxOut]
    escrowInputsOuts = byAddress escrowAddressLocal $ txInInfoResolved <$> txInfoInputs info

{-# INLINEABLE escrowValidator #-}
escrowValidator :: AuctionTerms -> Validator
escrowValidator terms =
  mkValidatorScript $
    $$(PlutusTx.compile [||wrapValidator . mkEscrowValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode (escrowAddress terms, standingBidAddress terms, feeEscrowAddress terms, terms)

{-# INLINEABLE escrowAddress #-}
escrowAddress :: AuctionTerms -> EscrowAddress
escrowAddress = EscrowAddress . validatorAddress . escrowValidator

-- Standing bid contract

mkStandingBidValidator :: AuctionTerms -> StandingBidDatum -> StandingBidRedeemer -> ScriptContext -> Bool
mkStandingBidValidator terms datum redeemer context =
  validAuctionTerms terms
    && case txInInfoResolved <$> txInfoInputs info of -- All cases require single input
      [inputOut] -> case redeemer of
        MoveToHydra ->
          -- FIXME: new requirements may appear in tech spec
          txInfoOutputs info == [inputOut] -- Check that nothing changed in output
        NewBid ->
          ( case txInfoOutputs info of
              [out] ->
                -- FIXME: Check bidder has right to make a bid
                traceIfFalse "Output is not into standing bid" $
                  txOutAddress out == scriptHashAddress (ownHash context)
                    && case validNewBid <$> decodeOutputDatum info inputOut <*> decodeOutputDatum info out of
                      Just x -> traceIfFalse "Incorrect bid" x
                      Nothing -> traceError "Incorrect encoding for input or output datum"
              _ -> traceError "Not exactly one ouput"
          )
            && interval 0 (biddingEnd terms) == txInfoValidRange info
        Cleanup ->
          traceIfFalse "Not exactly one voucher was burt during transaction" $
            let voucherAC = assetClass (standingBidVoucherCS datum) (stateTokenKindToTokenName Voucher)
             in assetClassValueOf (txInfoMint info) voucherAC == -1
      -- TODO: min ADA to seller
      _ : _ -> traceError "More than one input"
      [] -> traceError "Impossible happened: no inputs for staning bid validator"
  where
    info :: TxInfo
    info = scriptContextTxInfo context
    validNewBid :: StandingBidState -> StandingBidState -> Bool
    validNewBid oldBid (Bid newBidTerms) =
      case oldBid of
        Bid oldBidTerms ->
          traceIfFalse "Bid increment is not greater than minimumBidIncrement" $
            bidAmount oldBidTerms + minimumBidIncrement terms <= bidAmount newBidTerms
        NoBid ->
          traceIfFalse "Bid is not greater than startingBid" $
            startingBid terms <= bidAmount newBidTerms
    validNewBid _ NoBid = False

{-# INLINEABLE standingBidValidator #-}
standingBidValidator :: AuctionTerms -> Validator
standingBidValidator terms =
  mkValidatorScript $
    $$(PlutusTx.compile [||wrapValidator . mkStandingBidValidator||])
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
