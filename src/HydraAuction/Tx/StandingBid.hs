module HydraAuction.Tx.StandingBid (newBid, cleanupTx, newBidTx) where

-- Prelude imports

import Hydra.Prelude (void)
import Prelude

-- Plutus imports
import Plutus.V1.Ledger.Value (assetClassValue)
import Plutus.V2.Ledger.Api (getValidator)

-- Hydra imports
import Hydra.Cardano.Api (
  PlutusScriptV2,
  Tx,
  UTxO,
  fromPlutusScript,
  fromPlutusValue,
  lovelaceToValue,
  toPlutusKeyHash,
  verificationKeyHash,
  pattern ReferenceScriptNone,
  pattern ShelleyAddressInEra,
  pattern TxMintValueNone,
  pattern TxOut,
 )

-- TODO
import Cardano.Api.UTxO qualified as UTxO

-- Hydra auction imports
import HydraAuction.Addresses (VoucherCS (..))
import HydraAuction.OnChain (
  AuctionScript (StandingBid),
  policy,
  standingBidValidator,
  voucherAssetClass,
 )
import HydraAuction.Runner (Runner)
import HydraAuction.Tx.Common (
  AutoCreateParams (..),
  actorTipUtxo,
  addressAndKeys,
  autoCreateTx,
  autoSubmitAndAwaitTx,
  filterAdaOnlyUtxo,
  minLovelace,
  mkInlineDatum,
  mkInlinedDatumScriptWitness,
  scriptAddress,
  scriptPlutusScript,
  scriptUtxos,
 )
import HydraAuction.Tx.Escrow (toForgeStateToken)
import HydraAuction.Types (
  AuctionTerms (..),
  BidTerms (..),
  Natural,
  StandingBidDatum (..),
  StandingBidRedeemer (Cleanup, NewBid),
  StandingBidState (Bid),
  VoucherForgingRedeemer (BurnVoucher),
 )
import HydraAuctionUtils.Extras.Plutus (scriptCurrencySymbol)
import HydraAuctionUtils.Monads (logMsg)

newBid :: AuctionTerms -> Natural -> Runner ()
newBid terms bidAmount = do
  logMsg "Doing new bid"

  standingBidAddress <- scriptAddress StandingBid terms

  let txOutStandingBid bidderVk =
        TxOut
          (ShelleyAddressInEra standingBidAddress)
          valueStandingBid
          (mkInlineDatum datum)
          ReferenceScriptNone
        where
          mp = policy terms
          voucherCS = VoucherCS $ scriptCurrencySymbol mp
          datum =
            StandingBidDatum
              ( Bid $
                  BidTerms
                    (toPlutusKeyHash $ verificationKeyHash bidderVk)
                    bidAmount
              )
              voucherCS
          valueStandingBid =
            fromPlutusValue (assetClassValue (voucherAssetClass terms) 1)
              <> lovelaceToValue minLovelace
      standingBidWitness = mkInlinedDatumScriptWitness script NewBid
        where
          script = scriptPlutusScript StandingBid terms

  logMsg "Doing New bid"

  (bidderAddress, bidderVk, bidderSk) <- addressAndKeys

  bidderMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo
  standingBidUtxo <- scriptUtxos StandingBid terms

  -- FIXME: cover not proper UTxOs
  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { authoredUtxos =
            [ (bidderSk, bidderMoneyUtxo)
            ]
        , referenceUtxo = mempty
        , witnessedUtxos =
            [ (standingBidWitness, standingBidUtxo)
            ]
        , collateral = Nothing
        , outs = [txOutStandingBid bidderVk]
        , toMint = TxMintValueNone
        , changeAddress = bidderAddress
        , validityBound = (Just $ biddingStart terms, Just $ biddingEnd terms)
        }

newBidTx :: AuctionTerms -> Natural -> UTxO -> Runner Tx
newBidTx terms bidAmount standingBidUtxo = do
  logMsg "Doing new bid"

  standingBidAddress <- scriptAddress StandingBid terms

  let txOutStandingBid bidderVk =
        TxOut
          (ShelleyAddressInEra standingBidAddress)
          valueStandingBid
          (mkInlineDatum datum)
          ReferenceScriptNone
        where
          mp = policy terms
          voucherCS = VoucherCS $ scriptCurrencySymbol mp
          datum =
            StandingBidDatum
              ( Bid $
                  BidTerms
                    (toPlutusKeyHash $ verificationKeyHash bidderVk)
                    bidAmount
              )
              voucherCS
          valueStandingBid =
            fromPlutusValue (assetClassValue (voucherAssetClass terms) 1)
              <> lovelaceToValue minLovelace
      standingBidWitness = mkInlinedDatumScriptWitness script NewBid
        where
          script = scriptPlutusScript StandingBid terms

  (bidderAddress, bidderVk, _) <- addressAndKeys

  [(standingBidTxIn, _)] <- return $ UTxO.pairs $ standingBidUtxo

  -- FIXME: cover not proper UTxOs
  autoCreateTx False $
    AutoCreateParams
      { authoredUtxos = []
      , referenceUtxo = mempty
      , witnessedUtxos =
          [ (standingBidWitness, standingBidUtxo)
          ]
      , collateral = Just standingBidTxIn
      , outs = [txOutStandingBid bidderVk]
      , toMint = TxMintValueNone
      , changeAddress = bidderAddress
      , validityBound = (Nothing, Just $ biddingEnd terms)
      }

cleanupTx :: AuctionTerms -> Runner ()
cleanupTx terms = do
  logMsg "Doing standing bid cleanup"

  (actorAddress, _, actorSk) <- addressAndKeys

  standingBidUtxo <- scriptUtxos StandingBid terms
  actorMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo

  -- FIXME: cover not proper UTxOs
  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { authoredUtxos = [(actorSk, actorMoneyUtxo)]
        , referenceUtxo = mempty
        , witnessedUtxos =
            [ (standingBidWitness, standingBidUtxo)
            ]
        , collateral = Nothing
        , outs = []
        , toMint = toForgeStateToken terms BurnVoucher
        , changeAddress = actorAddress
        , validityBound = (Just $ cleanup terms, Nothing)
        }
  where
    standingBidWitness = mkInlinedDatumScriptWitness script Cleanup
      where
        script =
          fromPlutusScript @PlutusScriptV2 $
            getValidator $
              standingBidValidator terms
