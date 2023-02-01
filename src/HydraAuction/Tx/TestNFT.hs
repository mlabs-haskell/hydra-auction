module HydraAuction.Tx.TestNFT where

import Hydra.Prelude
import PlutusTx.Prelude (emptyByteString)

import qualified Data.List as List
import Cardano.Api.UTxO qualified as UTxO
import CardanoClient
import CardanoNode (RunningNode (..))
import Data.Maybe (fromJust)
import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Chain.CardanoClient
import Hydra.Cluster.Util (keysFor)
import HydraAuction.OnChain.TestNFT
import HydraAuction.Tx.Common hiding (actorAddress)
import Plutus.V1.Ledger.Value (AssetClass (..), assetClassValue, flattenValue)
import Plutus.V2.Ledger.Api (TokenName (..), getMintingPolicy, txOutValue)
import Hydra.Ledger.Cardano.Builder

mintOneTestNFT node@RunningNode {nodeSocket, networkId} actor = do
  (actorVk, actorSk) <- keysFor actor

  let actorAddress = buildAddress actorVk networkId
  putStrLn $ "Using actor: " <> show actor <> "with address: " <> show actorAddress

  utxo' <- actorTipUtxo node actor

  let pred x = (length <$> flattenValue <$> txOutValue <$> (toPlutusTxOut $ x)) == Just 1
      utxo = UTxO.filter pred utxo'
      (txIn, _) = fromJust $ viaNonEmpty last $ UTxO.pairs utxo

  let !valueOut = (fromPlutusValue $ assetClassValue allowMintingAssetClass 1) <> lovelaceToValue minLovelance
  let txOut = TxOut (ShelleyAddressInEra actorAddress) valueOut TxOutDatumNone ReferenceScriptNone
  let toMint = (mintedTokens (fromPlutusScript $ getMintingPolicy allowMintingPolicy) () [(tokenToAsset $ TokenName emptyByteString, 1)])

  void $ autoSubmitAndAwaitTx node $
    AutoCreateParams {
      authoredUtxos = [(actorSk, utxo)],
      otherUtxo = mempty,
      witnessedTxIns = [],
      collateral = Nothing,
      outs = [txOut],
      toMint = toMint,
      changeAddress = actorAddress
    }
