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

getUtxo node actor = do
  utxo' <- actorTipUtxo node actor

  let pred x = (length <$> flattenValue <$> txOutValue <$> (toPlutusTxOut $ x)) == Just 2
      utxo = UTxO.filter pred utxo'

  return utxo

constructTerms node@RunningNode {nodeSocket, networkId} actor utxoRef = do
  (actorVk, actorSk) <- keysFor actor
  let terms =
        AuctionTerms
          { auctionLot = allowMintingAssetClass
          , seller = toPlutusKeyHash $ verificationKeyHash actorVk
          , delegates = [toPlutusKeyHash $ verificationKeyHash actorVk]
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

announceAuction node@RunningNode {nodeSocket, networkId} actor terms utxoRef = do
  (actorVk, actorSk) <- keysFor actor

  let actorAddress = buildAddress actorVk networkId
  putStrLn $ "Using actor: " <> show actor <> "with address: " <> show actorAddress

  utxo <- queryUTxOByTxIn networkId nodeSocket QueryTip [utxoRef]
  utxoAll <- actorTipUtxo node actor

  let pred x = (length <$> flattenValue <$> txOutValue <$> (toPlutusTxOut $ x)) == Just 1
      utxoMoney = UTxO.filter pred utxoAll

  -- TODO: clean up

  let mp = policy terms
  let atDatum = AuctionEscrowDatum Announced (VoucherCS $ scriptCurrencySymbol mp)

  let voucherAssetClass = AssetClass (voucherCurrencySymbol terms, (stateTokenKindToTokenName Voucher))
  let toMint = (mintedTokens (fromPlutusScript $ getMintingPolicy mp) () [(tokenToAsset $ stateTokenKindToTokenName Voucher, 1)])
  let valueOut = (fromPlutusValue $ assetClassValue allowMintingAssetClass 1) <> (fromPlutusValue $ assetClassValue voucherAssetClass 1) <> lovelaceToValue minLovelance
  let !a = mkScriptAddress @PlutusScriptV2 networkId $ fromPlutusScript @PlutusScriptV2 $ getValidator $ escrowValidator terms
  let txOut = TxOut (a) valueOut (mkTxOutDatum atDatum) ReferenceScriptNone

  tx <- autoCreateTx node actorAddress actorSk [utxoRef] [txOut] (utxo <> utxoMoney) toMint
  putStrLn "Signed"

  submitTransaction networkId nodeSocket tx
  putStrLn "Submited"

  void $ awaitTransaction networkId nodeSocket tx
  putStrLn "Awaited"

  putStrLn $ "Created Tx id: " <> (show $ getTxId $ txBody tx)
