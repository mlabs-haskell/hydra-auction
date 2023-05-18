{-# OPTIONS_GHC -fno-specialise #-}

module HydraAuction.OnChain.StandingBid (
  mkStandingBidValidator,
  validNewBidTerms,
  sellerSignatureMessage,
  bidderSignatureMessage,
) where

-- Prelude imports
import PlutusTx.Prelude

-- Plutus imports
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Interval (contains, to)
import Plutus.V1.Ledger.Value (CurrencySymbol (..), assetClass, assetClassValueOf)
import Plutus.V2.Ledger.Api (
  PubKeyHash (..),
  TxInfo,
  TxOut (..),
  scriptContextTxInfo,
  txInInfoResolved,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoValidRange,
  txOutAddress,
 )
import Plutus.V2.Ledger.Contexts (ScriptContext, ownHash)

-- Hydra imporst
import Hydra.Contract.Head (hasPT)

-- Hydra auction imports
import HydraAuction.Addresses (VoucherCS (..))
import HydraAuction.OnChain.StateToken (StateTokenKind (..), stateTokenKindToTokenName)
import HydraAuction.Types (
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
              (contains (to (biddingEnd terms)) (txInfoValidRange info))
        Cleanup ->
          -- XXX: interval is checked on burning
          checkExactlyOneVoucherBurned
            && checkOutputIsToSeller
    _ : _ -> traceError "More than one standing bid input"
    [] -> traceError "Impossible happened: no inputs for staning bid validator"
  where
    info :: TxInfo
    info = scriptContextTxInfo context
    standingBidAddress = scriptHashAddress $ ownHash context
    inOutsByAddress address =
      byAddress address $ txInInfoResolved <$> txInfoInputs info
    validNewBid :: CurrencySymbol -> StandingBidState -> StandingBidState -> Bool
    validNewBid voucherCS (StandingBidState oldBid) (StandingBidState newBid) =
      validNewBidTerms terms voucherCS oldBid newBid
    checkCorrectNewBidOutput inputOut =
      case byAddress standingBidAddress $ txInfoOutputs info of
        [out] ->
          traceIfFalse
            "Output is not into standing bid"
            (txOutAddress out == scriptHashAddress (ownHash context))
            && checkValidNewBid out
        _ -> traceError "Not exactly one ouput"
      where
        checkValidNewBid out =
          let inDatum = decodeOutputDatum info inputOut
              inBid = standingBidState <$> inDatum
              Just inVoucherCS = standingBidVoucherCS <$> inDatum
              outBid = standingBidState <$> decodeOutputDatum info out
           in case validNewBid (unVoucherCS inVoucherCS) <$> inBid <*> outBid of
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
validNewBidTerms :: AuctionTerms -> CurrencySymbol -> Maybe BidTerms -> Maybe BidTerms -> Bool
validNewBidTerms terms voucherCS oldBid (Just newBidTerms) =
  -- The seller has allowed the bidder to participate in the auction
  traceIfFalse "User is not an autorised bider" (verifyEd25519Signature (sellerVK terms) sellerMessage sellerSignature)
    -- The bidder has correctly signed the datum
    && traceIfFalse "New bid datum is not signed correctly" (verifyEd25519Signature bidderVK bidderMessage bidderSignature)
    && case oldBid of
      Just oldBidTerms ->
        traceIfFalse "Bid increment is not greater than minimumBidIncrement" $
          bidAmount oldBidTerms + minimumBidIncrement terms <= bidAmount newBidTerms
      Nothing ->
        traceIfFalse "Bid is not greater than startingBid" $
          startingBid terms <= bidAmount newBidTerms
  where
    BidTerms bidderPKH bidderVK newPrice bidderSignature sellerSignature = newBidTerms
    auctionId = voucherCS
    sellerMessage = sellerSignatureMessage auctionId bidderVK bidderPKH
    bidderMessage = bidderSignatureMessage auctionId newPrice bidderPKH
validNewBidTerms _ _ _ Nothing =
  traceIfFalse "Bid cannot be empty" False

{-# INLINEABLE bidderSignatureMessage #-}
bidderSignatureMessage :: CurrencySymbol -> Natural -> PubKeyHash -> BuiltinByteString
bidderSignatureMessage (CurrencySymbol headId) bidAmount (PubKeyHash bidderPKH) = headId <> toBuiltinBytestring bidAmount <> bidderPKH

{-# INLINEABLE sellerSignatureMessage #-}
sellerSignatureMessage :: CurrencySymbol -> BuiltinByteString -> PubKeyHash -> BuiltinByteString
sellerSignatureMessage (CurrencySymbol headId) bidderVK (PubKeyHash bidderPKH) = headId <> bidderPKH <> bidderVK
