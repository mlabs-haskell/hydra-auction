module HydraAuction.OnChain.StandingBid (
  mkStandingBidValidator,
  validNewBidTerms,
  sellerSignatureMessage,
  bidderSignatureMessage,
) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports

import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), assetClass, assetClassValueOf)
import PlutusLedgerApi.V2 (PubKeyHash (..), txInInfoResolved)
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (..),
  TxInfo (..),
  findOwnInput,
 )
import PlutusLedgerApi.V2.Tx (
  TxOut (..),
  txOutAddress,
 )

-- Hydra imporst
import Hydra.Contract.Head (hasPT)

-- Hydra auction imports
import HydraAuction.Addresses (VoucherCS (..))
import HydraAuction.OnChain.Common (checkInterval)
import HydraAuction.OnChain.StateToken (StateTokenKind (..), stateTokenKindToTokenName)
import HydraAuction.Types (
  AuctionStage (..),
  AuctionTerms (..),
  BidTerms (..),
  StandingBidDatum (..),
  StandingBidRedeemer (..),
  StandingBidState (..),
 )
import HydraAuctionUtils.Plutus (
  byAddress,
  decodeOutputDatum,
  isNotAdaOnlyOutput,
  nothingForged,
 )
import HydraAuctionUtils.Types.Natural (Natural, toBuiltinBytestring)

{-# INLINEABLE mkStandingBidValidator #-}
mkStandingBidValidator :: AuctionTerms -> StandingBidDatum -> StandingBidRedeemer -> ScriptContext -> Bool
mkStandingBidValidator terms datum redeemer context =
  -- All cases require single standing bid input
  case inOutsByAddress standingBidAddress of
    [standingBidInOut] ->
      case redeemer of
        MoveToHydra ->
          case filter isNotAdaOnlyOutput $ txInfoOutputs info of
            [standingBidOutput] ->
              -- Lot cannot be stolen, cuz we have only one output
              -- Hydra validator shoud check that datum is not changed
              -- in commited output
              -- Also validator checks that output address is correct
              -- The only thing we should check is Tx has right PT
              traceIfFalse "No Hydra Participation Token" $
                hasPT (hydraHeadId terms) standingBidOutput
            _ -> traceError "Wrong number of standing bid outputs"
            && nothingForged info
        NewBid ->
          checkCorrectNewBidOutput standingBidInOut
            && nothingForged info
            && traceIfFalse
              "Wrong interval for NewBid"
              (checkInterval terms BiddingStartedStage info)
        Cleanup ->
          -- XXX: interval is checked on burning
          checkExactlyOneVoucherBurned
            && checkOutputIsToSeller
    _ : _ -> traceError "More than one standing bid input"
    [] -> traceError "Impossible happened: no inputs for staning bid validator"
  where
    info :: TxInfo
    info = scriptContextTxInfo context
    standingBidAddress = case findOwnInput context of
      Just x -> txOutAddress $ txInInfoResolved x
      Nothing -> traceError "Impossible happened"
    inOutsByAddress address =
      byAddress address $ txInInfoResolved <$> txInfoInputs info
    validNewBid :: VoucherCS -> StandingBidState -> StandingBidState -> Bool
    validNewBid voucherCS (StandingBidState oldBid) (StandingBidState newBid) =
      validNewBidTerms terms voucherCS oldBid newBid
    checkCorrectNewBidOutput inputOut =
      case byAddress standingBidAddress $ txInfoOutputs info of
        [out] ->
          traceIfFalse
            "Output is not into standing bid"
            (txOutAddress out == standingBidAddress)
            && checkValidNewBid out
        _ -> traceError "Not exactly one ouput"
      where
        checkValidNewBid out =
          let inDatum = decodeOutputDatum info inputOut
              inBid = standingBidState <$> inDatum
              inVoucherCS = standingBidVoucherCS <$> inDatum
              outBid = standingBidState <$> decodeOutputDatum info out
           in case validNewBid <$> inVoucherCS <*> inBid <*> outBid of
                Just x -> traceIfFalse "Incorrect bid" x
                Nothing ->
                  traceError "Incorrect encoding for input or output datum"
    checkOutputIsToSeller = case txInfoOutputs info of
      [out] ->
        traceIfFalse
          "Output is not to seller"
          (txOutAddress out == pubKeyHashAddress (sellerPKH terms))
      _ -> traceError "Not exactly one ouput"
    checkExactlyOneVoucherBurned =
      traceIfFalse
        "Not exactly one voucher was burt during transaction"
        ( let cs = unVoucherCS $ standingBidVoucherCS datum
              voucherAC = assetClass cs (stateTokenKindToTokenName Voucher)
           in assetClassValueOf (txInfoMint info) voucherAC == -1
        )

{-# INLINEABLE validNewBidTerms #-}
validNewBidTerms :: AuctionTerms -> VoucherCS -> Maybe BidTerms -> Maybe BidTerms -> Bool
validNewBidTerms terms voucherCS oldBid (Just newBidTerms) =
  -- The seller has allowed the bidder to participate in the auction
  traceIfFalse "User is not an authorised bider" (verifyEd25519Signature (sellerVK terms) sellerMessage sellerSignature)
    -- The bidder has correctly signed the datum
    && traceIfFalse "New bid datum is not signed correctly" (verifyEd25519Signature bidderVK bidderMessage bidderSignature)
    && checkBidTerms terms oldBid newBidTerms
  where
    BidTerms bidderPKH bidderVK newPrice bidderSignature sellerSignature = newBidTerms
    sellerMessage = sellerSignatureMessage voucherCS bidderVK bidderPKH
    bidderMessage = bidderSignatureMessage voucherCS newPrice bidderPKH
validNewBidTerms _ _ _ Nothing =
  traceIfFalse "Bid cannot be empty" False

{-# INLINEABLE checkBidTerms #-}
checkBidTerms :: AuctionTerms -> Maybe BidTerms -> BidTerms -> Bool
checkBidTerms terms (Just oldBidTerms) newBidTerms =
  traceIfFalse "Bid increment is not greater than minimumBidIncrement" $
    bidAmount oldBidTerms + minimumBidIncrement terms <= bidAmount newBidTerms
checkBidTerms terms Nothing newBidTerms =
  traceIfFalse "Bid is not greater than startingBid" $
    startingBid terms <= bidAmount newBidTerms

{-# INLINEABLE bidderSignatureMessage #-}
bidderSignatureMessage :: VoucherCS -> Natural -> PubKeyHash -> BuiltinByteString
bidderSignatureMessage (VoucherCS (CurrencySymbol headId)) bidAmount (PubKeyHash bidderPKH) =
  headId <> toBuiltinBytestring bidAmount <> bidderPKH

{-# INLINEABLE sellerSignatureMessage #-}
sellerSignatureMessage :: VoucherCS -> BuiltinByteString -> PubKeyHash -> BuiltinByteString
sellerSignatureMessage (VoucherCS (CurrencySymbol headId)) bidderVK (PubKeyHash bidderPKH) =
  headId <> bidderPKH <> bidderVK
