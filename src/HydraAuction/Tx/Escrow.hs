module HydraAuction.Tx.Escrow (
  toForgeStateToken,
  announceAuction,
  startBidding,
  bidderBuys,
  sellerReclaims,
) where

-- Prelude imports
import PlutusTx.Prelude (emptyByteString)
import Prelude

-- Haskell imports
import Control.Monad (void)

-- Plutus imports
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Value (
  CurrencySymbol (..),
  assetClassValue,
  unAssetClass,
 )

-- Hydra imports
import Hydra.Cardano.Api (
  Lovelace (..),
  fromPlutusTxOutRef,
  fromPlutusValue,
  lovelaceToValue,
  toPlutusKeyHash,
  verificationKeyHash,
  pattern ReferenceScriptNone,
  pattern ShelleyAddressInEra,
  pattern TxMintValueNone,
  pattern TxOut,
  pattern TxOutDatumNone,
 )

-- Cardano node imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra auction imports
import HydraAuction.Addresses (VoucherCS (..))
import HydraAuction.OnChain (
  AuctionScript (..),
  voucherAssetClass,
  voucherCurrencySymbol,
 )
import HydraAuction.OnChain.Common (stageToInterval)
import HydraAuction.Tx.Common (
  scriptAddress,
  scriptPlutusScript,
  scriptUtxos,
  toForgeStateToken,
 )
import HydraAuction.Tx.Deposit (parseBidDepositDatum)
import HydraAuction.Tx.StandingBid (queryStandingBidDatum)
import HydraAuction.Types (
  AuctionEscrowDatum (..),
  AuctionStage (..),
  AuctionState (..),
  AuctionTerms (..),
  BidDepositDatum (..),
  BidDepositRedeemer (WinningBidder),
  BidTerms (..),
  EscrowRedeemer (..),
  StandingBidDatum (..),
  StandingBidState (..),
  VoucherForgingRedeemer (MintVoucher),
  calculateTotalFee,
 )
import HydraAuctionUtils.L1.Runner (L1Runner)
import HydraAuctionUtils.Monads (
  MonadQueryUtxo (queryUtxo),
  UtxoQuery (ByTxIns),
  fromPlutusAddressInMonad,
  logMsg,
 )
import HydraAuctionUtils.Monads.Actors (
  WithActorT,
  actorTipUtxo,
  addressAndKeys,
 )
import HydraAuctionUtils.Plutus (extendIntervalRight)
import HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoSubmitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.Build (
  minLovelace,
  mkInlineDatum,
  mkInlinedDatumScriptWitness,
 )
import HydraAuctionUtils.Tx.Utxo (
  filterAdaOnlyUtxo,
  filterUtxoByCurrencySymbols,
 )
import HydraAuctionUtils.Types.Natural (naturalToInt)

announceAuction :: AuctionTerms -> WithActorT L1Runner ()
announceAuction terms = do
  logMsg "Doing announce auction"

  escrowAddress <- scriptAddress Escrow terms

  let voucherCS = voucherCurrencySymbol terms
      escrowAnnouncedUtxo = AuctionEscrowDatum Announced voucherCS
      announcedEscrowTxOut =
        TxOut
          (ShelleyAddressInEra escrowAddress)
          valueWithLotAndStateToken
          (mkInlineDatum escrowAnnouncedUtxo)
          ReferenceScriptNone
      valueWithLotAndStateToken =
        fromPlutusValue (assetClassValue (auctionLot terms) 1)
          <> fromPlutusValue (assetClassValue (voucherAssetClass terms) 1)
          <> lovelaceToValue minLovelace

  (sellerAddress, _, sellerSk) <- addressAndKeys

  utxoWithLotNFT <-
    queryUtxo (ByTxIns [fromPlutusTxOutRef $ utxoNonce terms])

  sellerMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo

  case length utxoWithLotNFT of
    0 -> fail "Utxo with Lot was consumed or not created"
    1 -> pure ()
    _ -> fail "Impossible happened: multiple utxoRef Utxos"

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(sellerSk, utxoWithLotNFT <> sellerMoneyUtxo)]
        , additionalSigners = []
        , referenceUtxo = mempty
        , witnessedUtxos = []
        , collateral = Nothing
        , outs = [announcedEscrowTxOut]
        , toMint = toForgeStateToken terms MintVoucher
        , changeAddress = sellerAddress
        , validityBound = stageToInterval terms AnnouncedStage
        }

startBidding :: AuctionTerms -> WithActorT L1Runner ()
startBidding terms = do
  logMsg "Doing start bidding"

  let escrowScript = scriptPlutusScript Escrow terms

  standingAddress <- scriptAddress StandingBid terms
  escrowAddress <- scriptAddress Escrow terms

  let voucherCS = voucherCurrencySymbol terms
      biddingStartedDatum =
        AuctionEscrowDatum
          BiddingStarted
          voucherCS
      txOutEscrow =
        TxOut
          (ShelleyAddressInEra escrowAddress)
          valueOutEscrow
          (mkInlineDatum biddingStartedDatum)
          ReferenceScriptNone
      valueOutEscrow =
        fromPlutusValue (assetClassValue (auctionLot terms) 1)
          <> lovelaceToValue minLovelace
      txOutStandingBid =
        TxOut
          (ShelleyAddressInEra standingAddress)
          valueOutStanding
          (mkInlineDatum standingBidDatum)
          ReferenceScriptNone
      standingBidDatum = StandingBidDatum (StandingBidState Nothing) voucherCS
      valueOutStanding =
        fromPlutusValue (assetClassValue (voucherAssetClass terms) 1)
          <> lovelaceToValue minLovelace
      escrowWitness = mkInlinedDatumScriptWitness escrowScript StartBidding

  (sellerAddress, _, sellerSk) <- addressAndKeys

  sellerMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo

  let escrowAnnounceSymbols =
        [ fst $
            unAssetClass $
              auctionLot terms
        , unVoucherCS voucherCS
        , CurrencySymbol emptyByteString
        ]

  escrowAnnounceUtxo <-
    filterUtxoByCurrencySymbols
      escrowAnnounceSymbols
      <$> scriptUtxos Escrow terms

  case length escrowAnnounceUtxo of
    0 -> fail "Utxo with announced escrow was consumed or not created"
    1 -> pure ()
    _ -> fail "Cannot choose between multiple utxos with announced escrow"

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(sellerSk, sellerMoneyUtxo)]
        , additionalSigners = []
        , referenceUtxo = mempty
        , witnessedUtxos = [(escrowWitness, escrowAnnounceUtxo)]
        , collateral = Nothing
        , outs = [txOutStandingBid, txOutEscrow]
        , toMint = TxMintValueNone
        , changeAddress = sellerAddress
        , validityBound = stageToInterval terms BiddingStartedStage
        }

bidderBuys :: AuctionTerms -> WithActorT L1Runner ()
bidderBuys terms = do
  feeEscrowAddress <- scriptAddress FeeEscrow terms
  sellerAddress <-
    fromPlutusAddressInMonad $
      pubKeyHashAddress $
        sellerPKH terms

  -- FIXME: better error reporting
  Just (StandingBidDatum {standingBidState}) <- queryStandingBidDatum terms

  let txOutSellerGotBid =
        TxOut
          sellerAddress
          value
          TxOutDatumNone
          ReferenceScriptNone
        where
          value =
            lovelaceToValue $
              Lovelace $
                bidAmount' - calculateTotalFee terms
          bidAmount' = case standingBid standingBidState of
            (Just (BidTerms {bidAmount})) -> naturalToInt bidAmount
            Nothing -> error "Standing bid UTxO has no bid"
      txOutBidderGotLot bidderAddress =
        TxOut
          (ShelleyAddressInEra bidderAddress)
          valueBidderLot
          TxOutDatumNone
          ReferenceScriptNone
        where
          valueBidderLot =
            fromPlutusValue (assetClassValue (auctionLot terms) 1)
              <> lovelaceToValue minLovelace

      txOutFeeEscrow =
        TxOut (ShelleyAddressInEra feeEscrowAddress) value (mkInlineDatum ()) ReferenceScriptNone
        where
          value = lovelaceToValue $ Lovelace $ calculateTotalFee terms

      escrowWitness = mkInlinedDatumScriptWitness script BidderBuys
        where
          script = scriptPlutusScript Escrow terms

  logMsg "Doing Bidder Buy"

  (bidderAddress, bidderVk, bidderSk) <- addressAndKeys

  bidderMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo

  let escrowBiddingStartedSymbols =
        [ CurrencySymbol emptyByteString
        , fst $ unAssetClass $ auctionLot terms
        ]

  escrowBiddingStartedUtxo <-
    filterUtxoByCurrencySymbols
      escrowBiddingStartedSymbols
      <$> scriptUtxos Escrow terms

  standingBidUtxo <- scriptUtxos StandingBid terms

  allDeposits <- scriptUtxos Deposit terms

  let voucherCS = voucherCurrencySymbol terms
      expectedDatum = BidDepositDatum (toPlutusKeyHash $ verificationKeyHash bidderVk) voucherCS
      depositScript = scriptPlutusScript Deposit terms
      depositWitness = mkInlinedDatumScriptWitness depositScript WinningBidder
      bidderDeposit = case UTxO.find ((== expectedDatum) . parseBidDepositDatum) allDeposits of
        Nothing -> []
        Just deposit -> [(depositWitness, UTxO.singleton deposit)]
  -- FIXME: cover not proper UTxOs
  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(bidderSk, bidderMoneyUtxo)]
        , additionalSigners = []
        , referenceUtxo = standingBidUtxo
        , witnessedUtxos =
            (escrowWitness, escrowBiddingStartedUtxo) : bidderDeposit
        , collateral = Nothing
        , outs =
            [ txOutBidderGotLot bidderAddress
            , txOutSellerGotBid
            , txOutFeeEscrow
            ]
        , toMint = TxMintValueNone
        , changeAddress = bidderAddress
        , validityBound = stageToInterval terms BiddingEndedStage
        }

sellerReclaims :: AuctionTerms -> WithActorT L1Runner ()
sellerReclaims terms = do
  feeEscrowAddress <- scriptAddress FeeEscrow terms

  let escrowScript = scriptPlutusScript Escrow terms
      txOutSellerGotLot sellerAddress =
        TxOut
          (ShelleyAddressInEra sellerAddress)
          valueSellerLot
          TxOutDatumNone
          ReferenceScriptNone
        where
          valueSellerLot =
            fromPlutusValue (assetClassValue (auctionLot terms) 1)
              <> lovelaceToValue minLovelace

      escrowWitness = mkInlinedDatumScriptWitness escrowScript SellerReclaims

      txOutFeeEscrow =
        TxOut
          (ShelleyAddressInEra feeEscrowAddress)
          value
          (mkInlineDatum ())
          ReferenceScriptNone
        where
          value = lovelaceToValue $ Lovelace $ calculateTotalFee terms

  logMsg "Doing Seller reclaims"

  (sellerAddress, _, sellerSk) <- addressAndKeys

  sellerMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo

  let escrowBiddingStartedSymbols =
        [ CurrencySymbol emptyByteString
        , fst $ unAssetClass $ auctionLot terms
        ]

  escrowBiddingStartedUtxo <-
    filterUtxoByCurrencySymbols escrowBiddingStartedSymbols
      <$> scriptUtxos Escrow terms

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(sellerSk, sellerMoneyUtxo)]
        , additionalSigners = []
        , referenceUtxo = mempty
        , witnessedUtxos =
            [ (escrowWitness, escrowBiddingStartedUtxo)
            ]
        , collateral = Nothing
        , outs =
            [ txOutSellerGotLot sellerAddress
            , txOutFeeEscrow
            ]
        , toMint = TxMintValueNone
        , changeAddress = sellerAddress
        , validityBound = extendIntervalRight $ stageToInterval terms VoucherExpiredStage
        }
