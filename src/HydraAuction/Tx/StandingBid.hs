module HydraAuction.Tx.StandingBid (newBid) where

import Hydra.Prelude hiding (Natural)

import CardanoNode (RunningNode (..))
import Hydra.Cardano.Api hiding (txOutValue)
import Hydra.Cluster.Fixture (Actor)
import HydraAuction.Addresses
import HydraAuction.OnChain
import HydraAuction.PlutusExtras
import HydraAuction.Tx.Common
import HydraAuction.Types
import Plutus.V1.Ledger.Value (assetClassValue)
import Plutus.V2.Ledger.Api (getValidator)

newBid :: RunningNode -> Actor -> AuctionTerms -> Natural -> IO ()
newBid node bidder terms bidAmount = do
  putStrLn "Doing new bid"
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
        , referenceUtxo = mempty
        , witnessedUtxos =
            [ (standingBidWitness, standingBidUtxo)
            ]
        , collateral = Nothing
        , outs = [txOutStandingBid bidderVk]
        , toMint = TxMintValueNone
        , changeAddress = bidderAddress
        , validityBound = (Just $ biddingStart terms, Just $ biddingEnd terms)
        }
  where
    txOutStandingBid bidderVk = TxOut standingBidAddress' valueStandingBid (mkInlineDatum datum) ReferenceScriptNone
      where
        mp = policy terms
        voucherCS = VoucherCS $ scriptCurrencySymbol mp
        datum = StandingBidDatum (Bid (BidTerms (toPlutusKeyHash $ verificationKeyHash bidderVk) bidAmount)) voucherCS
        standingBidAddress' = mkScriptAddress @PlutusScriptV2 (networkId node) $ fromPlutusScript @PlutusScriptV2 $ getValidator $ standingBidValidator terms
        valueStandingBid =
          fromPlutusValue (assetClassValue (voucherAssetClass terms) 1)
            <> lovelaceToValue minLovelace
    standingBidWitness = mkInlinedDatumScriptWitness script NewBid
      where
        script = fromPlutusScript @PlutusScriptV2 $ getValidator $ standingBidValidator terms
