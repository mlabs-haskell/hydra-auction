module HydraAuction.Tx.TestNFT (mintOneTestNFT, findTestNFT) where

-- Prelude imports
import Prelude

-- Haskell imports
import Data.Map qualified as Map

-- Plutus imports
import PlutusLedgerApi.V1.Value (assetClassValue, valueOf)
import PlutusLedgerApi.V2 (always)
import PlutusLedgerApi.V2.Tx (txOutValue)

-- Hydra imports
import Cardano.Api.UTxO qualified as UTxO
import Hydra.Cardano.Api (
  Tx,
  TxIn,
  fromPlutusScript,
  fromPlutusValue,
  lovelaceToValue,
  toPlutusTxOut,
  pattern ReferenceScriptNone,
  pattern ShelleyAddressInEra,
  pattern TxOut,
  pattern TxOutDatumNone,
 )

-- Hydra auction imports
import HydraAuction.OnChain.TestNFT (
  testNftAssetClass,
  testNftCurrencySymbol,
  testNftPolicy,
  testNftTokenName,
 )
import HydraAuctionUtils.L1.Runner (L1Runner)
import HydraAuctionUtils.Monads.Actors (
  actorTipUtxo,
  addressAndKeys,
 )
import HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoSubmitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.Build (
  minLovelace,
  mintedTokens,
  tokenToAsset,
 )
import HydraAuctionUtils.Tx.Utxo (
  filterAdaOnlyUtxo,
 )

findTestNFT :: UTxO.UTxO -> Maybe TxIn
findTestNFT (UTxO.UTxO m) = Map.foldrWithKey isTestNFT Nothing m
  where
    isTestNFT k v acc = do
      txOut <- toPlutusTxOut v
      if valueOf (txOutValue txOut) testNftCurrencySymbol testNftTokenName == 1
        then pure k
        else acc

mintOneTestNFT :: L1Runner Tx
mintOneTestNFT = do
  (actorAddress, _, actorSk) <- addressAndKeys

  actorMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo

  let valueOut =
        fromPlutusValue (assetClassValue testNftAssetClass 1)
          <> lovelaceToValue minLovelace

      txOut =
        TxOut
          (ShelleyAddressInEra actorAddress)
          valueOut
          TxOutDatumNone
          ReferenceScriptNone

      toMint =
        mintedTokens
          (fromPlutusScript testNftPolicy)
          ()
          [(tokenToAsset testNftTokenName, 1)]

  autoSubmitAndAwaitTx $
    AutoCreateParams
      { signedUtxos = [(actorSk, actorMoneyUtxo)]
      , additionalSigners = []
      , referenceUtxo = mempty
      , witnessedUtxos = []
      , collateral = Nothing
      , outs = [txOut]
      , toMint = toMint
      , changeAddress = actorAddress
      , validityBound = always
      }
