{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Tx.Escrow (
  announceAuction,
  startBidding,
  bidderBuys,
  sellerReclaims,
) where

import Hydra.Prelude
import PlutusTx.Prelude (emptyByteString)

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (QueryPoint (QueryTip), queryUTxOByTxIn)
import CardanoNode (RunningNode (..))
import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Cluster.Fixture (Actor (..))
import HydraAuction.Addresses
import HydraAuction.OnChain
import HydraAuction.OnChain.StateToken
import HydraAuction.PlutusExtras
import HydraAuction.Runner
import HydraAuction.Tx.Common
import HydraAuction.Types
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.V2.Ledger.Api (
  fromData,
  getMintingPolicy,
  getValidator,
 )
import Plutus.V1.Ledger.Value (
  CurrencySymbol (..),
  assetClassValue,
  unAssetClass,
 )

announceAuction :: Actor -> AuctionTerms -> Runner ()
announceAuction sellerActor terms = do
  MkExecutionContext {..} <- ask
  let networkId' = networkId node
      nodeSocket' = nodeSocket node

  let mp = policy terms
      voucerCS = VoucherCS $ scriptCurrencySymbol mp
      escrowAnnouncedUtxo = AuctionEscrowDatum Announced voucerCS
      announcedEscrowTxOut =
        TxOut
          escrowAddress'
          valueWithLotAndStateToken
          (mkInlineDatum escrowAnnouncedUtxo)
          ReferenceScriptNone
      valueWithLotAndStateToken =
        fromPlutusValue (assetClassValue (auctionLot terms) 1)
          <> fromPlutusValue (assetClassValue (voucherAssetClass terms) 1)
          <> lovelaceToValue minLovelace
      escrowAddress' =
        mkScriptAddress @PlutusScriptV2 networkId' $
          fromPlutusScript @PlutusScriptV2 $
            getValidator $ escrowValidator terms

      toMintStateToken =
        mintedTokens
          (fromPlutusScript $ getMintingPolicy mp)
          ()
          [(tokenToAsset $ stateTokenKindToTokenName Voucher, 1)]

  (sellerAddress, _, sellerSk) <-
    addressAndKeysFor sellerActor

  utxoWithLotNFT <-
    liftIO $
      queryUTxOByTxIn
        networkId'
        nodeSocket'
        QueryTip
        [fromPlutusTxOutRef $ utxoRef terms]

  sellerMoneyUtxo <-
    liftIO $
      filterAdaOnlyUtxo <$> actorTipUtxo node sellerActor

  case length utxoWithLotNFT of
    0 -> fail "Utxo with Lot was consumed or not created"
    1 -> pure ()
    _ -> fail "Impossible happened: multiple utxoRef Utxos"

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { authoredUtxos = [(sellerSk, utxoWithLotNFT <> sellerMoneyUtxo)]
        , referenceUtxo = mempty
        , witnessedUtxos = []
        , collateral = Nothing
        , outs = [announcedEscrowTxOut]
        , toMint = toMintStateToken
        , changeAddress = sellerAddress
        , validityBound = (Just $ biddingStart terms, Just $ biddingEnd terms)
        }

startBidding :: Actor -> AuctionTerms -> Runner ()
startBidding sellerActor terms = do
  liftIO $ putStrLn "Doing start bidding"
  MkExecutionContext {..} <- ask
  let networkId' = networkId node

  let mp = policy terms
      escrowScript = fromPlutusScript $ getValidator $ escrowValidator terms
      voucherCS = VoucherCS $ scriptCurrencySymbol mp
      biddingStartedDatum =
        AuctionEscrowDatum
          (BiddingStarted (ApprovedBiddersHash emptyByteString))
          voucherCS
      txOutEscrow =
        TxOut
          escrowAddress'
          valueOutEscrow
          (mkInlineDatum biddingStartedDatum)
          ReferenceScriptNone
      valueOutEscrow =
        fromPlutusValue (assetClassValue (auctionLot terms) 1)
          <> lovelaceToValue minLovelace
      escrowAddress' = mkScriptAddress @PlutusScriptV2 networkId' escrowScript
      txOutStandingBid =
        TxOut
          standingAddress
          valueOutStanding
          (mkInlineDatum standingBidDatum)
          ReferenceScriptNone
      standingBidDatum = StandingBidDatum NoBid voucherCS
      valueOutStanding =
        fromPlutusValue (assetClassValue (voucherAssetClass terms) 1)
          <> lovelaceToValue minLovelace
      standingAddress =
        mkScriptAddress @PlutusScriptV2 networkId' $
          fromPlutusScript @PlutusScriptV2 $
            getValidator $ standingBidValidator terms
      escrowWitness = mkInlinedDatumScriptWitness escrowScript StartBidding

  (sellerAddress, _, sellerSk) <-
    addressAndKeysFor sellerActor

  sellerMoneyUtxo <-
    liftIO $
      filterAdaOnlyUtxo <$> actorTipUtxo node sellerActor

  let escrowAnnounceSymbols =
        [ fst $
            unAssetClass $
              auctionLot terms
        , scriptCurrencySymbol mp
        , CurrencySymbol emptyByteString
        ]

  escrowAnnounceUtxo <-
    liftIO $
      filterUtxoByCurrencySymbols
        escrowAnnounceSymbols
        <$> scriptUtxos node Escrow terms

  case length escrowAnnounceUtxo of
    0 -> fail "Utxo with announced escrow was consumed or not created"
    1 -> pure ()
    _ -> fail "Cannot choose between multiple utxos with announced escrow"

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { authoredUtxos = [(sellerSk, sellerMoneyUtxo)]
        , referenceUtxo = mempty
        , witnessedUtxos = [(escrowWitness, escrowAnnounceUtxo)]
        , collateral = Nothing
        , outs = [txOutStandingBid, txOutEscrow]
        , toMint = TxMintValueNone
        , changeAddress = sellerAddress
        , validityBound = (Just $ biddingStart terms, Just $ biddingEnd terms)
        }

bidderBuys :: Actor -> AuctionTerms -> Runner ()
bidderBuys bidder terms = do
  MkExecutionContext {..} <- ask
  let networkId' = networkId node

      txOutSellerGotBid standingBidUtxo =
        TxOut
          ( fromPlutusAddress (networkIdToNetwork networkId') $
              pubKeyHashAddress $ seller terms
          )
          value
          TxOutDatumNone
          ReferenceScriptNone
        where
          value =
            lovelaceToValue $
              Lovelace $ bidAmount' - naturalToInt (auctionFee terms)
          bidAmount' = case UTxO.pairs standingBidUtxo of
            [(_, out)] -> case txOutDatum out of
              TxOutDatumInline scriptData ->
                case fromData $ toPlutusData scriptData of
                  Just (StandingBidDatum {standingBidState}) ->
                    case standingBidState of
                      (Bid (BidTerms {bidAmount})) -> naturalToInt bidAmount
                      NoBid -> error "Standing bid UTxO has no bid"
                  Nothing ->
                    error "Impossible happened: Cannot decode standing bid datum"
              _ -> error "Impossible happened: No inline data for standing bid"
            _ -> error "Wrong number of standing bid UTxOs found"

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
        TxOut feeEscrowAddress value TxOutDatumNone ReferenceScriptNone
        where
          value = lovelaceToValue $ Lovelace $ naturalToInt $ auctionFee terms
          feeEscrowAddress =
            mkScriptAddress @PlutusScriptV2 networkId' $
              fromPlutusScript @PlutusScriptV2 $
                getValidator $ feeEscrowValidator terms

      escrowWitness = mkInlinedDatumScriptWitness script BidderBuys
        where
          script =
            fromPlutusScript @PlutusScriptV2 $
              getValidator $ escrowValidator terms

  logMsg "Doing Bidder Buy"

  (bidderAddress, _, bidderSk) <-
    addressAndKeysFor bidder

  bidderMoneyUtxo <-
    liftIO $
      filterAdaOnlyUtxo <$> actorTipUtxo node bidder

  let escrowBiddingStartedSymbols =
        [ CurrencySymbol emptyByteString
        , fst $ unAssetClass $ auctionLot terms
        ]

  escrowBiddingStartedUtxo <-
    liftIO $
      filterUtxoByCurrencySymbols
        escrowBiddingStartedSymbols
        <$> scriptUtxos node Escrow terms

  standingBidUtxo <-
    liftIO $
      scriptUtxos node StandingBid terms

  -- FIXME: cover not proper UTxOs

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { authoredUtxos = [(bidderSk, bidderMoneyUtxo)]
        , referenceUtxo = standingBidUtxo
        , witnessedUtxos =
            [ (escrowWitness, escrowBiddingStartedUtxo)
            ]
        , collateral = Nothing
        , outs =
            [ txOutBidderGotLot bidderAddress
            , txOutSellerGotBid standingBidUtxo
            , txOutFeeEscrow
            ]
        , toMint = TxMintValueNone
        , changeAddress = bidderAddress
        , validityBound = (Just $ biddingEnd terms, Just $ voucherExpiry terms)
        }

sellerReclaims :: Actor -> AuctionTerms -> Runner ()
sellerReclaims seller terms = do
  MkExecutionContext {..} <- ask
  let networkId' = networkId node

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

      escrowWitness = mkInlinedDatumScriptWitness script SellerReclaims
        where
          script =
            fromPlutusScript @PlutusScriptV2 $
              getValidator $ escrowValidator terms

      txOutFeeEscrow =
        TxOut
          feeEscrowAddress
          value
          TxOutDatumNone
          ReferenceScriptNone
        where
          value = lovelaceToValue $ Lovelace $ naturalToInt $ auctionFee terms
          feeEscrowAddress =
            mkScriptAddress @PlutusScriptV2 networkId' $
              fromPlutusScript @PlutusScriptV2 $
                getValidator $ feeEscrowValidator terms

  logMsg "Doing Seller reclaims"

  (sellerAddress, _, sellerSk) <-
    addressAndKeysFor seller

  sellerMoneyUtxo <-
    liftIO $
      filterAdaOnlyUtxo <$> actorTipUtxo node seller

  let escrowBiddingStartedSymbols =
        [ CurrencySymbol emptyByteString
        , fst $ unAssetClass $ auctionLot terms
        ]

  escrowBiddingStartedUtxo <-
    liftIO $
      filterUtxoByCurrencySymbols escrowBiddingStartedSymbols
        <$> scriptUtxos node Escrow terms

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { authoredUtxos = [(sellerSk, sellerMoneyUtxo)]
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
        , validityBound = (Just $ voucherExpiry terms, Nothing)
        }
