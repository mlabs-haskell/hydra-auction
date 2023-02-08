{-# LANGUAGE RecordWildCards #-}

module HydraAuction.Tx.Escrow (AuctionTermsConfig (..), constructTerms, announceAuction, startBidding, bidderBuys, sellerReclaims) where

import Hydra.Prelude hiding (Natural)
import PlutusTx.Prelude (emptyByteString)

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient
import CardanoNode (RunningNode (..))
import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (keysFor)
import HydraAuction.Addresses
import HydraAuction.OnChain
import HydraAuction.OnChain.StateToken
import HydraAuction.OnChain.TestNFT
import HydraAuction.PlutusExtras
import HydraAuction.Tx.Common
import HydraAuction.Types
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.V1.Ledger.Value (CurrencySymbol (..), assetClassValue, unAssetClass)
import Plutus.V2.Ledger.Api (POSIXTime (..), fromData, getMintingPolicy, getValidator)

data AuctionTermsConfig = AuctionTermsConfig
  { deltaBiddingStart :: Natural
  , deltaBiddingEnd :: Natural
  , deltaVoucherExpiry :: Natural
  , deltaCleanup :: Natural
  , configAuctionFee :: Natural
  , configStartingBid :: Natural
  , configMinIncrement :: Natural
  }
  deriving stock (Generic, Show, Eq)

-- fromInteger converts an integer number of seconds to a POSIXTime
natToPosix :: Natural -> POSIXTime
natToPosix = fromInteger . unNatural

constructTerms :: RunningNode -> POSIXTime -> AuctionTermsConfig -> Actor -> TxIn -> IO AuctionTerms
constructTerms _ currentTime AuctionTermsConfig {..} seller utxoRef = do
  (sellerVk, _) <- keysFor seller
  let sellerVkHash = toPlutusKeyHash $ verificationKeyHash sellerVk
      terms =
        AuctionTerms
          { auctionLot = allowMintingAssetClass
          , seller = sellerVkHash
          , delegates = [sellerVkHash]
          , biddingStart = currentTime + natToPosix deltaBiddingStart
          , biddingEnd = currentTime + natToPosix deltaBiddingEnd
          , voucherExpiry = currentTime + natToPosix deltaVoucherExpiry
          , cleanup = currentTime + natToPosix deltaCleanup
          , auctionFee = configAuctionFee
          , startingBid = configStartingBid
          , minimumBidIncrement = configMinIncrement
          , utxoRef = toPlutusTxOutRef utxoRef
          }
  return terms

announceAuction :: RunningNode -> Actor -> AuctionTerms -> IO ()
announceAuction node@RunningNode {networkId, nodeSocket} sellerActor terms = do
  putStrLn "Doing announce auction"
  (sellerAddress, _, sellerSk) <- addressAndKeysFor networkId sellerActor

  utxoWithLotNFT <- queryUTxOByTxIn networkId nodeSocket QueryTip [fromPlutusTxOutRef $ utxoRef terms]
  sellerMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo node sellerActor

  case length utxoWithLotNFT of
    0 -> error "Utxo with Lot was consumed or not created"
    1 -> return ()
    _ -> error "Impossible happened: multiple utxoRef Utxos"

  void $
    autoSubmitAndAwaitTx node $
      AutoCreateParams
        { authoredUtxos = [(sellerSk, utxoWithLotNFT <> sellerMoneyUtxo)]
        , referenceUtxo = mempty
        , witnessedUtxos = []
        , collateral = Nothing
        , outs = [announcedEscrowTxOut]
        , toMint = toMintStateToken
        , changeAddress = sellerAddress
        , validityBound = (Nothing, Just $ biddingStart terms)
        }
  where
    mp = policy terms
    voucerCS = VoucherCS $ scriptCurrencySymbol mp
    escrowAnnouncedUtxo = AuctionEscrowDatum Announced voucerCS
    announcedEscrowTxOut = TxOut escrowAddress' valueWithLotAndStateToken (mkInlineDatum escrowAnnouncedUtxo) ReferenceScriptNone
      where
        valueWithLotAndStateToken =
          fromPlutusValue (assetClassValue (auctionLot terms) 1)
            <> fromPlutusValue (assetClassValue (voucherAssetClass terms) 1)
            <> lovelaceToValue minLovelace
        escrowAddress' = mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript @PlutusScriptV2 $ getValidator $ escrowValidator terms
    toMintStateToken = mintedTokens (fromPlutusScript $ getMintingPolicy mp) () [(tokenToAsset $ stateTokenKindToTokenName Voucher, 1)]

startBidding :: RunningNode -> Actor -> AuctionTerms -> IO ()
startBidding node@RunningNode {networkId} sellerActor terms = do
  putStrLn "Doing start bidding"

  (sellerAddress, _, sellerSk) <- addressAndKeysFor networkId sellerActor

  sellerMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo node sellerActor
  let escrowAnnounceSymbols = [fst $ unAssetClass $ auctionLot terms, scriptCurrencySymbol mp, CurrencySymbol emptyByteString]
  escrowAnnounceUtxo <- filterUtxoByCurrencySymbols escrowAnnounceSymbols <$> scriptUtxos node Escrow terms

  case length escrowAnnounceUtxo of
    0 -> error "Utxo with announced escrow was consumed or not created"
    1 -> return ()
    _ -> error "Cannot choose between multiple utxos with announced escrow"

  void $
    autoSubmitAndAwaitTx node $
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
  where
    escrowScript = fromPlutusScript $ getValidator $ escrowValidator terms
    mp = policy terms
    voucherCS = VoucherCS $ scriptCurrencySymbol mp
    biddingStartedDatum = AuctionEscrowDatum (BiddingStarted (ApprovedBiddersHash emptyByteString)) voucherCS
    txOutEscrow = TxOut escrowAddress' valueOutEscrow (mkInlineDatum biddingStartedDatum) ReferenceScriptNone
      where
        valueOutEscrow =
          fromPlutusValue (assetClassValue (auctionLot terms) 1)
            <> lovelaceToValue minLovelace
        escrowAddress' = mkScriptAddress @PlutusScriptV2 networkId escrowScript
    txOutStandingBid = TxOut standingAddress valueOutStanding (mkInlineDatum standingBidDatum) ReferenceScriptNone
      where
        standingBidDatum = StandingBidDatum NoBid voucherCS
        valueOutStanding =
          fromPlutusValue (assetClassValue (voucherAssetClass terms) 1)
            <> lovelaceToValue minLovelace
        standingAddress = mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript @PlutusScriptV2 $ getValidator $ standingBidValidator terms
    escrowWitness = mkInlinedDatumScriptWitness escrowScript StartBidding

bidderBuys :: RunningNode -> Actor -> AuctionTerms -> IO ()
bidderBuys node@RunningNode {networkId} bidder terms = do
  putStrLn "Doing Bidder Buy"
  (bidderAddress, _, bidderSk) <- addressAndKeysFor networkId bidder

  bidderMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo node bidder
  let escrowBiddingStartedSymbols = [CurrencySymbol emptyByteString, fst $ unAssetClass $ auctionLot terms]
  escrowBiddingStartedUtxo <- filterUtxoByCurrencySymbols escrowBiddingStartedSymbols <$> scriptUtxos node Escrow terms
  standingBidUtxo <- scriptUtxos node StandingBid terms

  -- FIXME: cover not proper UTxOs

  void $
    autoSubmitAndAwaitTx node $
      AutoCreateParams
        { authoredUtxos = [(bidderSk, bidderMoneyUtxo)]
        , referenceUtxo = standingBidUtxo
        , witnessedUtxos =
            [ (escrowWitness, escrowBiddingStartedUtxo)
            ]
        , collateral = Nothing
        , outs = [txOutBidderGotLot bidderAddress, txOutSellerGotBid standingBidUtxo, txOutFeeEscrow]
        , toMint = TxMintValueNone
        , changeAddress = bidderAddress
        , validityBound = (Just $ biddingEnd terms, Just $ voucherExpiry terms)
        }
  where
    txOutSellerGotBid standingBidUtxo = TxOut (fromPlutusAddress (networkIdToNetwork networkId) $ pubKeyHashAddress $ seller terms) value TxOutDatumNone ReferenceScriptNone
      where
        bidAmount' = case UTxO.pairs standingBidUtxo of
          [(_, out)] -> case txOutDatum out of
            TxOutDatumInline scriptData -> case fromData $ toPlutusData scriptData of
              Just (StandingBidDatum {standingBidState}) ->
                case standingBidState of
                  (Bid (BidTerms {bidAmount})) -> naturalToInt bidAmount
                  NoBid -> error "Standing bid UTxO has no bid"
              Nothing -> error "Impossible happened: Cannot decode standing bid datum"
            _ -> error "Impossible happened: No inline data for standing bid"
          _ -> error "Wrong number of standing bid UTxOs found"
        value = lovelaceToValue $ Lovelace $ bidAmount' - naturalToInt (auctionFee terms)
    txOutBidderGotLot bidderAddress = TxOut (ShelleyAddressInEra bidderAddress) valueBidderLot TxOutDatumNone ReferenceScriptNone
      where
        valueBidderLot =
          fromPlutusValue (assetClassValue (auctionLot terms) 1)
            <> lovelaceToValue minLovelace
    txOutFeeEscrow = TxOut feeEscrowAddress value TxOutDatumNone ReferenceScriptNone
      where
        feeEscrowAddress = mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript @PlutusScriptV2 $ getValidator $ feeEscrowValidator terms
        value = lovelaceToValue $ Lovelace $ naturalToInt $ auctionFee terms
    escrowWitness = mkInlinedDatumScriptWitness script BidderBuys
      where
        script = fromPlutusScript @PlutusScriptV2 $ getValidator $ escrowValidator terms

sellerReclaims :: RunningNode -> Actor -> AuctionTerms -> IO ()
sellerReclaims node@RunningNode {networkId} seller terms = do
  putStrLn "Doing Seller reclaims"
  (sellerAddress, _, sellerSk) <- addressAndKeysFor networkId seller

  sellerMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo node seller

  let escrowBiddingStartedSymbols = [CurrencySymbol emptyByteString, fst $ unAssetClass $ auctionLot terms]
  escrowBiddingStartedUtxo <- filterUtxoByCurrencySymbols escrowBiddingStartedSymbols <$> scriptUtxos node Escrow terms

  void $
    autoSubmitAndAwaitTx node $
      AutoCreateParams
        { authoredUtxos = [(sellerSk, sellerMoneyUtxo)]
        , referenceUtxo = mempty
        , witnessedUtxos =
            [ (escrowWitness, escrowBiddingStartedUtxo)
            ]
        , collateral = Nothing
        , outs = [txOutSellerGotLot sellerAddress, txOutFeeEscrow] -- TODO: fee escrow
        , toMint = TxMintValueNone
        , changeAddress = sellerAddress
        , validityBound = (Just $ voucherExpiry terms, Nothing)
        }
  where
    txOutSellerGotLot sellerAddress = TxOut (ShelleyAddressInEra sellerAddress) valueSellerLot TxOutDatumNone ReferenceScriptNone
      where
        valueSellerLot =
          fromPlutusValue (assetClassValue (auctionLot terms) 1)
            <> lovelaceToValue minLovelace
    escrowWitness = mkInlinedDatumScriptWitness script SellerReclaims
      where
        script = fromPlutusScript @PlutusScriptV2 $ getValidator $ escrowValidator terms

    txOutFeeEscrow = TxOut feeEscrowAddress value TxOutDatumNone ReferenceScriptNone
      where
        feeEscrowAddress = mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript @PlutusScriptV2 $ getValidator $ feeEscrowValidator terms
        value = lovelaceToValue $ Lovelace $ naturalToInt $ auctionFee terms
