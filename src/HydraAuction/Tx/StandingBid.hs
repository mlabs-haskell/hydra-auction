module HydraAuction.Tx.StandingBid where

import Hydra.Prelude hiding (Natural)
import PlutusTx.Prelude (emptyByteString)

import Cardano.Api.UTxO qualified as UTxO
import CardanoNode (RunningNode (..))
import Data.Maybe (fromJust)
import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Cluster.Fixture (Actor)
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
import Plutus.V1.Ledger.Value (AssetClass (..), CurrencySymbol (..), assetClassValue, flattenValue, symbols)
import Plutus.V2.Ledger.Api (POSIXTime (..), TokenName (..), getMintingPolicy, getValidator, toBuiltinData, toData, txOutValue)

newBid :: RunningNode -> Actor -> AuctionTerms -> Natural -> IO ()
newBid node bidder terms bidAmount = do
  putStrLn "Doing Bidder Buy"
  (bidderAddress, bidderVk, bidderSk) <- addressAndKeysFor (networkId node) bidder

  bidderMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo node bidder
  standingBidUtxo <- scriptUtxos node StandingBid terms

  -- FIXME: cover not proper UTxOs

  void $
    autoSubmitAndAwaitTx node $
      AutoCreateParams
        { authoredUtxos =
            [ (bidderSk, bidderMoneyUtxo)
            ]
        , witnessedUtxos =
            [ (standingBidWitness, standingBidUtxo)
            ]
        , collateral = Nothing
        , outs = [txOutStandingBid bidderVk]
        , toMint = TxMintValueNone
        , changeAddress = bidderAddress
        }
  where
    txOutStandingBid bidderVk = TxOut standingBidAddress valueStandingBid (mkInlineDatum $ datum bidderVk) ReferenceScriptNone
      where
        mp = policy terms
        voucherCS = VoucherCS $ scriptCurrencySymbol mp
        datum bidderVk = StandingBidDatum (Bid (BidTerms (toPlutusKeyHash $ verificationKeyHash bidderVk) bidAmount)) voucherCS
        standingBidAddress = mkScriptAddress @PlutusScriptV2 (networkId node) $ fromPlutusScript @PlutusScriptV2 $ getValidator $ standingBidValidator terms
        valueStandingBid =
          (fromPlutusValue $ assetClassValue (voucherAssetClass terms) 1)
            <> lovelaceToValue minLovelance
    standingBidWitness = mkInlinedDatumScriptWitness script NewBid
      where
        script = fromPlutusScript @PlutusScriptV2 $ getValidator $ standingBidValidator terms
