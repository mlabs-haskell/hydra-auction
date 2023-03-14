module HydraAuction.Tx.TestNFT (mintOneTestNFT, findTestNFT) where

-- Prelude imports

import Hydra.Prelude (liftIO)
import Prelude

-- Haskell imports
import Data.Map qualified as Map

-- Plutus imports
import Plutus.V1.Ledger.Value (assetClassValue, valueOf)
import Plutus.V2.Ledger.Api (getMintingPolicy, txOutValue)

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
import HydraAuction.Runner (Runner, logMsg)
import HydraAuction.Tx.Common (
  AutoCreateParams (..),
  actorTipUtxo,
  addressAndKeys,
  autoSubmitAndAwaitTx,
  filterAdaOnlyUtxo,
  minLovelace,
  mintedTokens,
  tokenToAsset,
 )

findTestNFT :: UTxO.UTxO -> Maybe TxIn
findTestNFT (UTxO.UTxO m) = Map.foldrWithKey isTestNFT Nothing m
  where
    isTestNFT k v acc = do
      txOut <- toPlutusTxOut v
      if valueOf (txOutValue txOut) testNftCurrencySymbol testNftTokenName == 1
        then pure k
        else acc

mintOneTestNFT :: Runner Tx
mintOneTestNFT = do
  logMsg "Doing Minting Test NFT"

  (actorAddress, _, actorSk) <- addressAndKeys

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
          (fromPlutusScript $ getMintingPolicy testNftPolicy)
          ()
          [(tokenToAsset testNftTokenName, 1)]

  actorMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo
  -- let actorMoneyUtxo = UTxO.fromPairs [head (UTxO.pairs actorMoneyUtxo')]

  liftIO $ putStrLn $ show actorMoneyUtxo

  autoSubmitAndAwaitTx $
    AutoCreateParams
      { authoredUtxos = [(actorSk, actorMoneyUtxo)]
      , referenceUtxo = mempty
      , witnessedUtxos = []
      , collateral = Nothing
      , outs = [txOut]
      , toMint = toMint
      , changeAddress = actorAddress
      , validityBound = (Nothing, Nothing)
      }
