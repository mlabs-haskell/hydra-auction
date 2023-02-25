module HydraAuction.Tx.TestNFT (mintOneTestNFT) where

-- Prelude imports
import Prelude

-- Plutus imports
import Plutus.V1.Ledger.Value (assetClassValue)
import Plutus.V2.Ledger.Api (getMintingPolicy)

-- Hydra imports
import Hydra.Cardano.Api hiding (txOutValue)

-- Hydra auction imports
import HydraAuction.OnChain.TestNFT (
  testNftAssetClass,
  testNftPolicy,
  testNftTokenName,
 )
import HydraAuction.Runner (Runner)
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

mintOneTestNFT :: Runner Tx
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
          (fromPlutusScript $ getMintingPolicy testNftPolicy)
          ()
          [(tokenToAsset testNftTokenName, 1)]

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
