module HydraAuction.Tx.TestNFT where

import Hydra.Prelude
import PlutusTx.Prelude (emptyByteString)

import Cardano.Api.UTxO qualified as UTxO
import CardanoNode (RunningNode (..))
import Data.List qualified as List
import Data.Maybe (fromJust)
import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Cluster.Fixture (Actor)
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger.Cardano.Builder
import HydraAuction.OnChain.TestNFT
import HydraAuction.Tx.Common hiding (actorAddress)
import Plutus.V1.Ledger.Value (AssetClass (..), assetClassValue, flattenValue)
import Plutus.V2.Ledger.Api (TokenName (..), getMintingPolicy, txOutValue)

mintOneTestNFT :: RunningNode -> Actor -> IO Tx
mintOneTestNFT node actor = do
  (actorAddress, actorVk, actorSk) <- addressAndKeysFor (networkId node) actor

  actorMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo node actor

  let valueOut = fromPlutusValue (assetClassValue allowMintingAssetClass 1) <> lovelaceToValue minLovelance
  let txOut = TxOut (ShelleyAddressInEra actorAddress) valueOut TxOutDatumNone ReferenceScriptNone
  let toMint =
        mintedTokens (fromPlutusScript $ getMintingPolicy allowMintingPolicy) () [(tokenToAsset $ TokenName emptyByteString, 1)]

  autoSubmitAndAwaitTx node $
    AutoCreateParams
      { authoredUtxos = [(actorSk, actorMoneyUtxo)]
      , witnessedUtxos = []
      , collateral = Nothing
      , outs = [txOut]
      , toMint = toMint
      , changeAddress = actorAddress
      }
