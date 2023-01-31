module HydraAuction.Tx.Escrow where

import Hydra.Prelude
import PlutusTx.Prelude (emptyByteString)

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient
import CardanoNode (RunningNode (..))
import Data.Maybe (fromJust)
import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Chain.CardanoClient
import Hydra.Cluster.Util (keysFor)
import HydraAuction.Addresses
import HydraAuction.OnChain
import HydraAuction.OnChain.Escrow
import HydraAuction.OnChain.StateToken
import HydraAuction.OnChain.TestNFT
import HydraAuction.PlutusExtras
import HydraAuction.Tx.Common hiding (actorAddress)
import HydraAuction.Tx.TestNFT
import HydraAuction.Types
import Plutus.V1.Ledger.Value (AssetClass (..), assetClassValue, flattenValue)
import Plutus.V2.Ledger.Api (POSIXTime (..), TokenName (..), getMintingPolicy, getValidator, toBuiltinData, toData, txOutValue)

constructTerms node@RunningNode {nodeSocket, networkId} actor = do
  (actorVk, actorSk) <- keysFor actor
  utxo <- queryUTxOFor networkId nodeSocket QueryTip actorVk
  -- TODO: filter
  let (txIn, _) = fromJust $ viaNonEmpty last $ UTxO.pairs utxo
  let terms =
        AuctionTerms
          { auctionLot = allowMintingAssetClass
          , seller = toPlutusKeyHash $ verificationKeyHash actorVk
          , delegates = []
          , biddingStart = POSIXTime 0
          , biddingEnd = POSIXTime 100
          , voucherExpiry = POSIXTime 1000
          , cleanup = POSIXTime 10001
          , auctionFee = fromJust $ intToNatural 2_000_000
          , startingBid = fromJust $ intToNatural 2_000_000
          , minimumBidIncrement = fromJust $ intToNatural 2_000_000
          , utxoRef = toPlutusTxOutRef txIn
          }
  return terms

announceAuction node@RunningNode {nodeSocket, networkId} actor terms = do
  (actorVk, actorSk) <- keysFor actor

  let actorAddress = buildAddress actorVk networkId
  putStrLn $ "Using actor: " <> show actor <> "with address: " <> show actorAddress

  utxo' <- actorTipUtxo node actor

  let !valueOut = (fromPlutusValue $ assetClassValue allowMintingAssetClass 1) <> lovelaceToValue minLovelance
  let !txOut1 = TxOut (ShelleyAddressInEra actorAddress) valueOut TxOutDatumNone ReferenceScriptNone
  -- TODO: filter utxos
  let pred x = (length <$> flattenValue <$> txOutValue <$> (toPlutusTxOut $ x)) == Just 2
      utxo = UTxO.filter pred utxo'
      (txIn, _) = fromJust $ viaNonEmpty last $ UTxO.pairs utxo

  putStrLn $ show utxo

  -- TODO: clean up

  let mp = policy terms
  let !atDatum' = AuctionEscrowDatum Announced (VoucherCS $ scriptCurrencySymbol mp)
  let atDatum = toData $ toBuiltinData $ atDatum'
  let !x = fromPlutusData atDatum
  let atDatumHash = hashScriptData x

  let voucherAssetClass = AssetClass (voucherCurrencySymbol terms, (stateTokenKindToTokenName Voucher))
  let toMint = (mintedTokens (fromPlutusScript $ getMintingPolicy mp) () [(tokenToAsset $ stateTokenKindToTokenName Voucher, 1)])
  let valueOut = (fromPlutusValue $ assetClassValue allowMintingAssetClass 1) <> (fromPlutusValue $ assetClassValue voucherAssetClass 1) <> lovelaceToValue minLovelance
  let !a = mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript @PlutusScriptV2 $ getValidator $ escrowValidator terms
  let !txOut = TxOut (a) valueOut (TxOutDatumHash atDatumHash) ReferenceScriptNone -- TODO: filter utxos
  tx <- autoCreateTx node actorAddress actorSk [txIn] [txOut1] utxo toMint
  putStrLn "Signed"

  submitTransaction networkId nodeSocket tx
  putStrLn "Submited"

  slot <- queryTipSlotNo networkId nodeSocket
  putStrLn $ show slot
  void $ awaitTransaction networkId nodeSocket tx
  putStrLn "Awaited"

-- TODO: return utxo
