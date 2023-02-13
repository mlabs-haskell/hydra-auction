module HydraAuction.Tx.TestNFT (mintOneTestNFT) where

import Hydra.Prelude
import PlutusTx.Prelude (emptyByteString)

import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Cluster.Fixture (Actor)
import HydraAuction.OnChain.TestNFT
import HydraAuction.Runner
import HydraAuction.Tx.Common
import Plutus.V1.Ledger.Value (assetClassValue)
import Plutus.V2.Ledger.Api (TokenName (..), getMintingPolicy)

mintOneTestNFT :: Actor -> Runner Tx
mintOneTestNFT actor = do
  (actorAddress, _, actorSk) <-
    addressAndKeysFor actor

  actorMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo actor

  let valueOut =
        fromPlutusValue (assetClassValue allowMintingAssetClass 1)
          <> lovelaceToValue minLovelace

      txOut =
        TxOut
          (ShelleyAddressInEra actorAddress)
          valueOut
          TxOutDatumNone
          ReferenceScriptNone

      toMint =
        mintedTokens
          (fromPlutusScript $ getMintingPolicy allowMintingPolicy)
          ()
          [(tokenToAsset $ TokenName emptyByteString, 1)]

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
