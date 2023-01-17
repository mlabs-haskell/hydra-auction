{-# LANGUAGE RecordWildCards #-}

module HydraAuction.OnChain (mkPolicy, voucherCurrencySymbol, mkEscrowValidator, escrowAddress, standingBidAddress) where

import PlutusTx.Prelude

import HydraAuction.Addresses
import HydraAuction.Types
import Plutus.Extras
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Contexts
import Plutus.V1.Ledger.Value (assetClassValueOf, flattenValue)
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
    && minimumBidIncrement > 0
    && startingBid > auctionFee
    && auctionFee > length delegates * minAuctionFee
    && not (null delegates)
    && modulo auctionFee (length delegates) == 0

-- TODO: check interval from TxInfo
validAuctionTerms :: AuctionTerms -> Bool
validAuctionTerms terms = validAuctionTerms' terms (POSIXTime 0)

{-# INLINEABLE decodeOutputDatum #-}
decodeOutputDatum :: PlutusTx.FromData a => TxInfo -> TxOut -> Maybe a
decodeOutputDatum info output = do
  hash <- txOutDatumHash output
  datum <- findDatum hash info
  fromBuiltinData $ getDatum datum

-- State Tokens

data StateTokenKind = Voucher

{-# INLINEABLE stateTokenKindToTokenName #-}
stateTokenKindToTokenName :: StateTokenKind -> TokenName
stateTokenKindToTokenName Voucher = TokenName "Voucher"

{-# INLINEABLE mkPolicy #-}
mkPolicy :: (EscrowAddress, AuctionTerms) -> () -> ScriptContext -> Bool
mkPolicy (EscrowAddress escrowAddressLocal, terms) () ctx =
  traceIfFalse "AuctionTerms is invalid" $
    validAuctionTerms terms
      && exactlyUtxoRefConsumed
      && exactlyOneOutputToEscrow
      && if anyOurTokenForged
        then
          traceIfFalse
            "Not exactly each one of state token forged"
            exactlyOneOfEachTokenForged
        else traceIfFalse "Imposible happened: nothing forged" False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    ourTokensForged =
      Map.fromList
        [ (tn, amount)
        | (cs, tn, amount) <- flattenValue (txInfoMint info)
        , ownCurrencySymbol ctx == cs
        ]
    anyOurTokenForged :: Bool
    anyOurTokenForged = ourTokensForged /= Map.empty
    exactlyOneOfEachTokenForged :: Bool
    exactlyOneOfEachTokenForged =
      ourTokensForged
        == Map.fromList
          [ (stateTokenKindToTokenName Voucher, 1)
          ]
    exactlyUtxoRefConsumed :: Bool
    exactlyUtxoRefConsumed = case txInfoInputs info of
      [out] ->
        traceIfFalse "Input is not equal to utxoRef" (txInInfoOutRef out == utxoRef terms)
          && traceIfFalse
            "Input does not contain auction lot"
            (assetClassValueOf (txOutValue $ txInInfoResolved out) (auctionLot terms) == 1)
      _ -> traceError "Inputs are not exactly single input"
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

mkEscrowValidator :: (EscrowAddress, StandingBidAddress, AuctionTerms) -> AuctionEscrowDatum -> () -> ScriptContext -> Bool
mkEscrowValidator (EscrowAddress escrowAddressLocal, StandingBidAddress standingBidAddressLocal, terms) _ _ context =
  traceIfFalse "AuctionTerms is invalid" $
    validAuctionTerms terms
      && case escrowInputsOuts of
        [output] -> case auctionState <$> decodeOutputDatum info output of
          Just Announced ->
            let outputs = txInfoOutputs info
             in traceError "Not three outputs" $
                  length outputs == 3
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
          Just _ -> traceError "This auction state not supported yet"
          Nothing -> traceError "Could not decode auction state"
        _ : _ -> traceError "More than one escrow input"
        [] -> traceError "Imposible happened: no escrow inputs"
  where
    info :: TxInfo
    info = scriptContextTxInfo context
    byAddress :: Address -> [TxOut] -> [TxOut]
    byAddress address = filter (\o -> txOutAddress o == address)
    escrowInputsOuts :: [TxOut]
    escrowInputsOuts = txInInfoResolved <$> txInfoInputs info

{-# INLINEABLE escrowValidator #-}
escrowValidator :: AuctionTerms -> Validator
escrowValidator terms =
  mkValidatorScript $
    $$(PlutusTx.compile [||wrapValidator . mkEscrowValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode (escrowAddress terms, standingBidAddress terms, terms)

{-# INLINEABLE escrowAddress #-}
escrowAddress :: AuctionTerms -> EscrowAddress
escrowAddress = EscrowAddress . validatorAddress . escrowValidator

-- Standing bid contract

{-# INLINEABLE standingBidAddress #-}
standingBidAddress :: AuctionTerms -> StandingBidAddress
standingBidAddress _ = traceError "TODO"
