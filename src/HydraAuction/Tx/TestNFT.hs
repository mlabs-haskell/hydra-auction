module HydraAuction.Tx.TestNFT (mintOneTestNFT) where

import Hydra.Prelude
import PlutusTx.Prelude (emptyByteString)

import CardanoNode (RunningNode (..))
import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Cluster.Fixture (Actor)
import HydraAuction.OnChain.TestNFT
import HydraAuction.Tx.Common
import Plutus.V1.Ledger.Value (assetClassValue)
import Plutus.V2.Ledger.Api (TokenName (..), getMintingPolicy)

mintOneTestNFT :: RunningNode -> Actor -> IO Tx
mintOneTestNFT node actor = do
  (actorAddress, _, actorSk) <- addressAndKeysFor (networkId node) actor

  actorMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo node actor

  let valueOut = fromPlutusValue (assetClassValue allowMintingAssetClass 1) <> lovelaceToValue minLovelace
  let txOut = TxOut (ShelleyAddressInEra actorAddress) valueOut TxOutDatumNone ReferenceScriptNone
  let toMint =
        mintedTokens (fromPlutusScript $ getMintingPolicy allowMintingPolicy) () [(tokenToAsset $ TokenName emptyByteString, 1)]

  autoSubmitAndAwaitTx node $
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
