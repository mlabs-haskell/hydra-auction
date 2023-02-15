module HydraAuction.Tx.TestNFT (mintOneTestNFT) where

-- Prelude imports
import Hydra.Prelude

-- Plutus imports
import Plutus.V1.Ledger.Value (assetClassValue)
import Plutus.V2.Ledger.Api (getMintingPolicy)

-- Hydra imports
import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Cluster.Fixture (Actor)

-- Hydra auction imports
import HydraAuction.OnChain.TestNFT
import HydraAuction.Runner
import HydraAuction.Tx.Common

mintOneTestNFT :: Actor -> Runner Tx
mintOneTestNFT actor = do
  (actorAddress, _, actorSk) <-
    addressAndKeysFor actor

  actorMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo actor

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
