module HydraAuction.Tx.Escrow where

import Hydra.Prelude
import PlutusTx.Prelude (emptyByteString)

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient
import CardanoNode (RunningNode (..))
import Data.Maybe (fromJust)
import Hydra.Cardano.Api hiding (txOutValue, Testnet)
import Hydra.Chain.CardanoClient
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (keysFor)
import HydraAuction.Addresses
import HydraAuction.Types
import HydraAuction.OnChain
import HydraAuction.OnChain.Escrow
import HydraAuction.OnChain.StateToken
import HydraAuction.OnChain.TestNFT
import HydraAuction.PlutusExtras
import HydraAuction.Tx.Common hiding (sellerAddress)
import HydraAuction.Tx.TestNFT
import HydraAuction.Types
import Cardano.Ledger.BaseTypes (Network(Testnet))
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.V1.Ledger.Value (AssetClass (..), CurrencySymbol (..), assetClassValue, flattenValue, symbols)
import Plutus.V2.Ledger.Api (POSIXTime (..), TokenName (..), getMintingPolicy, getValidator, toBuiltinData, toData, txOutValue)

constructTerms :: RunningNode -> Actor -> TxIn -> IO AuctionTerms
constructTerms node@RunningNode {networkId} seller utxoRef = do
  (sellerVk, sellerSk) <- keysFor seller
  let sellerVkHash = toPlutusKeyHash $ verificationKeyHash sellerVk
      terms =
        AuctionTerms
          { auctionLot = allowMintingAssetClass
          , seller = sellerVkHash
          , delegates = [sellerVkHash]
          , biddingStart = POSIXTime 1
          , biddingEnd = POSIXTime 100
          , voucherExpiry = POSIXTime 1000
          , cleanup = POSIXTime 10001
          , auctionFee = fromJust $ intToNatural 4_000_000
          , startingBid = fromJust $ intToNatural 8_000_000
          , minimumBidIncrement = fromJust $ intToNatural 8_000_000
          , utxoRef = toPlutusTxOutRef utxoRef
          }
  return terms

announceAuction :: RunningNode -> Actor -> AuctionTerms -> IO ()
announceAuction node@RunningNode {networkId, nodeSocket} seller terms = do
  (sellerAddress, _, sellerSk) <- addressAndKeysFor networkId seller

  utxoWithLotNFT <- queryUTxOByTxIn networkId nodeSocket QueryTip [fromPlutusTxOutRef $ utxoRef terms]
  sellerMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo node seller

  case (length utxoWithLotNFT) of
    0 -> error "Utxo with Lot was consumed or not created"
    1 -> return ()
    _ -> error "Impossible happened: multiple utxoRef Utxos"

  void $
    autoSubmitAndAwaitTx node $
      AutoCreateParams
        { authoredUtxos = [(sellerSk, utxoWithLotNFT <> sellerMoneyUtxo)]
        , witnessedUtxos = []
        , collateral = Nothing
        , outs = [announcedEscrowTxOut]
        , toMint = toMintStateToken
        , changeAddress = sellerAddress
        }
  where
    mp = policy terms
    voucerCS = VoucherCS $ scriptCurrencySymbol mp
    escrowAnnouncedUtxo = AuctionEscrowDatum Announced voucerCS
    announcedEscrowTxOut = TxOut escrowAddress valueWithLotAndStateToken (mkInlineDatum escrowAnnouncedUtxo) ReferenceScriptNone
      where
        valueWithLotAndStateToken =
          (fromPlutusValue $ assetClassValue (auctionLot terms) 1)
            <> (fromPlutusValue $ assetClassValue (voucherAssetClass terms) 1)
            <> lovelaceToValue minLovelance
        escrowAddress = mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript @PlutusScriptV2 $ getValidator $ escrowValidator terms
    toMintStateToken = (mintedTokens (fromPlutusScript $ getMintingPolicy mp) () [(tokenToAsset $ stateTokenKindToTokenName Voucher, 1)])

startBidding :: RunningNode -> Actor -> AuctionTerms -> IO ()
startBidding node@RunningNode {networkId} seller terms = do
  (sellerAddress, _, sellerSk) <- addressAndKeysFor networkId seller

  sellerMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo node seller
  let escrowAnnounceSymbols = [allowMintingCurrencySymbol, scriptCurrencySymbol mp, CurrencySymbol emptyByteString]
  escrowAnnounceUtxo <- filterUtxoByCurrencySymbols escrowAnnounceSymbols <$> scriptUtxos node Escrow terms

  case (length escrowAnnounceUtxo) of
    0 -> error "Utxo with announced escrow was consumed or not created"
    1 -> return ()
    _ -> error "Cannot choose between multiple utxos with announced escrow"

  void $
    autoSubmitAndAwaitTx node $
      AutoCreateParams
        { authoredUtxos = [(sellerSk, sellerMoneyUtxo)]
        , witnessedUtxos = [(escrowWitness, escrowAnnounceUtxo)]
        , collateral = Nothing
        , outs = [txOutStandingBid, txOutEscrow]
        , toMint = TxMintValueNone
        , changeAddress = sellerAddress
        }
  where
    escrowScript = fromPlutusScript $ getValidator $ escrowValidator terms
    mp = policy terms
    voucherCS = VoucherCS $ scriptCurrencySymbol mp
    biddingStartedDatum = AuctionEscrowDatum (BiddingStarted (ApprovedBiddersHash emptyByteString)) voucherCS
    txOutEscrow = TxOut escrowAddress valueOutEscrow (mkInlineDatum biddingStartedDatum) ReferenceScriptNone
      where
        valueOutEscrow =
          (fromPlutusValue $ assetClassValue (auctionLot terms) 1)
            <> lovelaceToValue minLovelance
        escrowAddress = mkScriptAddress @PlutusScriptV2 networkId escrowScript
    txOutStandingBid = TxOut standingAddress valueOutStanding (mkInlineDatum standingBidDatum) ReferenceScriptNone
      where
        standingBidDatum = StandingBidDatum NoBid voucherCS
        valueOutStanding =
          (fromPlutusValue $ assetClassValue (voucherAssetClass terms) 1)
          <> lovelaceToValue minLovelance
        standingAddress = mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript @PlutusScriptV2 $ getValidator $ standingBidValidator terms
    escrowWitness = mkInlinedDatumScriptWitness escrowScript StartBidding

bidderBuys :: RunningNode -> Actor -> AuctionTerms -> IO ()
bidderBuys node@RunningNode {networkId} bidder terms = do
  putStrLn "Doing Bidder Buy"
  (bidderAddress, _, bidderSk) <- addressAndKeysFor networkId bidder

  bidderMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo node bidder
  -- TODO: allowMInting
  let escrowBiddingStartedSymbols = [CurrencySymbol emptyByteString, allowMintingCurrencySymbol]
  escrowBiddingStartedUtxo <- filterUtxoByCurrencySymbols escrowBiddingStartedSymbols <$> scriptUtxos node Escrow terms
  standingBidUtxo <- scriptUtxos node StandingBid terms

  -- FIXME: cover not proper UTxOs

  putStrLn $ show escrowBiddingStartedUtxo
  putStrLn $ show standingBidUtxo

  void $
    autoSubmitAndAwaitTx node $
      AutoCreateParams
        { authoredUtxos = [
          (bidderSk, bidderMoneyUtxo)
          ]
        , witnessedUtxos = [
          (escrowWitness, escrowBiddingStartedUtxo),
          (standingBidWitness, standingBidUtxo)
          ]
        , collateral = Nothing
        , outs = [txOutBidderGotLot bidderAddress, txOutStandingBid, txOutSellerGotBid]
        , toMint = TxMintValueNone
        , changeAddress = bidderAddress
        }

  where
    mp = policy terms
    -- TODO: decode bid and actually send it
    -- TODO: decode plutus address on Mainnet too
    txOutSellerGotBid = TxOut (fromPlutusAddress Testnet $ pubKeyHashAddress $ seller terms) (lovelaceToValue 16_000_000) TxOutDatumNone ReferenceScriptNone
    txOutBidderGotLot bidderAddress = TxOut (ShelleyAddressInEra bidderAddress) valueBidderLot TxOutDatumNone ReferenceScriptNone
      where
        valueBidderLot =
          (fromPlutusValue $ assetClassValue (auctionLot terms) 1)
          <> lovelaceToValue minLovelance
    txOutStandingBid = TxOut standingBidAddress valueStandingBid TxOutDatumNone ReferenceScriptNone
      where
        standingBidAddress = mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript @PlutusScriptV2 $ getValidator $ standingBidValidator terms
        valueStandingBid =
          (fromPlutusValue $ assetClassValue (voucherAssetClass terms) 1)
          <> lovelaceToValue minLovelance
    escrowWitness = mkInlinedDatumScriptWitness script BidderBuys
      where
        script = fromPlutusScript @PlutusScriptV2 $ getValidator $ escrowValidator terms
    standingBidWitness = mkInlinedDatumScriptWitness script UseBid
      where
        script = fromPlutusScript @PlutusScriptV2 $ getValidator $ standingBidValidator terms
