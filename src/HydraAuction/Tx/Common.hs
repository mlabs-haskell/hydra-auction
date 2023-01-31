module HydraAuction.Tx.Common where

import Prelude

import Cardano.Api.UTxO as UTxO
import CardanoClient hiding (networkId)
import CardanoNode (
  RunningNode (RunningNode, networkId, nodeSocket),
 )
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Contravariant (contramap)
import Data.Map.Internal qualified as Map
import Data.Tuple.Extra (first, second)
import Hydra.Cardano.Api
import Hydra.Cluster.Faucet
import Hydra.Cluster.Fixture
import Hydra.Cluster.Util
import HydraNode
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api (
  POSIXTime (POSIXTime),
  PubKeyHash (PubKeyHash),
  fromBuiltin,
  toBuiltin,
 )

minLovelance :: Lovelace
minLovelance = 2_000_000

tokenToAsset :: TokenName -> AssetName
tokenToAsset (TokenName t) = AssetName $ fromBuiltin t

mintedTokens :: ToScriptData redeemer => PlutusScript -> redeemer -> [(AssetName, Quantity)] -> TxMintValue BuildTx
mintedTokens script redeemer assets =
  TxMintValue mintedTokens' mintedWitnesses'
  where
    mintedTokens' = valueFromList (fmap (first (AssetId policyId)) assets)
    mintedWitnesses' =
      BuildTxWith $ Map.singleton policyId mintingWitness
    mintingWitness :: ScriptWitness WitCtxMint
    mintingWitness =
      mkScriptWitness script NoScriptDatumForMint (toScriptData redeemer)
    policyId =
      PolicyId $ hashScript $ PlutusScript script

actorTipUtxo :: RunningNode -> Actor -> IO UTxO.UTxO
actorTipUtxo node actor = do
  (vk, _) <- keysFor actor
  liftIO $ queryUTxOFor (networkId node) (nodeSocket node) QueryTip vk
