module HydraAuction.Tx.StandingBid (
  newBid,
  cleanupTx,
  getStadingBidDatum,
  currentWinningBidder,
  getApprovedBidders,
  newBid',
) where

-- Prelude imports

import Hydra.Prelude (MonadIO, void)
import Prelude

-- Haskell imports
import Control.Monad (when)

-- Plutus imports
import Plutus.V1.Ledger.Value (assetClassValue)
import Plutus.V2.Ledger.Api (PubKeyHash, fromData, getValidator)

-- Hydra imports
import Cardano.Api.UTxO qualified as UTxO
import Hydra.Cardano.Api (
  PlutusScriptV2,
  fromPlutusScript,
  fromPlutusValue,
  lovelaceToValue,
  toPlutusData,
  toPlutusKeyHash,
  txOutDatum,
  verificationKeyHash,
  pattern ReferenceScriptNone,
  pattern ShelleyAddressInEra,
  pattern TxMintValueNone,
  pattern TxOut,
  pattern TxOutDatumInline,
 )

-- Hydra auction imports

import Control.Monad.Reader (MonadReader (ask))
import HydraAuction.Addresses (VoucherCS (..))
import HydraAuction.OnChain (
  AuctionScript (StandingBid),
  policy,
  standingBidValidator,
  voucherAssetClass,
 )
import HydraAuction.Runner (ExecutionContext (..), Runner)
import HydraAuction.Tx.Common (
  actorTipUtxo,
  addressAndKeys,
  minLovelace,
  mkInlineDatum,
  mkInlinedDatumScriptWitness,
  scriptAddress,
  scriptPlutusScript,
  scriptUtxos,
  toForgeStateToken,
 )
import HydraAuction.Types (
  ApprovedBidders,
  AuctionTerms (..),
  BidTerms (..),
  Natural,
  StandingBidDatum (..),
  StandingBidRedeemer (Cleanup, NewBid),
  StandingBidState (..),
  VoucherForgingRedeemer (BurnVoucher),
 )
import HydraAuctionUtils.Extras.Plutus (scriptCurrencySymbol)
import HydraAuctionUtils.Fixture (Actor)
import HydraAuctionUtils.Monads (
  MonadCardanoClient,
  MonadNetworkId,
  MonadQueryUtxo (..),
  MonadTrace,
  UtxoQuery (..),
  addressAndKeysForActor,
  logMsg,
 )
import HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoSubmitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.Utxo (
  filterAdaOnlyUtxo,
 )

getStadingBidDatum :: UTxO.UTxO -> StandingBidDatum
getStadingBidDatum standingBidUtxo =
  case UTxO.pairs standingBidUtxo of
    [(_, out)] -> case txOutDatum out of
      TxOutDatumInline scriptData ->
        case fromData $ toPlutusData scriptData of
          Just standingBidDatum -> standingBidDatum
          Nothing ->
            error "Impossible happened: Cannot decode standing bid datum"
      _ -> error "Impossible happened: No inline data for standing bid"
    _ -> error "Wrong number of standing bid UTxOs found"

currentWinningBidder :: (MonadNetworkId m, MonadQueryUtxo m) => AuctionTerms -> m (Maybe PubKeyHash)
currentWinningBidder terms = do
  standingBidUtxo <- scriptUtxos StandingBid terms
  let StandingBidDatum {standingBidState} = getStadingBidDatum standingBidUtxo
  return $ case standingBid standingBidState of
    (Just (BidTerms {bidBidder})) -> Just bidBidder
    Nothing -> Nothing

getApprovedBidders :: (MonadNetworkId m, MonadQueryUtxo m) => AuctionTerms -> m ApprovedBidders
getApprovedBidders terms = do
  standingBidUtxo <- scriptUtxos StandingBid terms
  let StandingBidDatum {standingBidState} = getStadingBidDatum standingBidUtxo
  pure $ approvedBidders standingBidState

newBid :: AuctionTerms -> Natural -> Runner ()
newBid terms bidAmount = do
  MkExecutionContext {actor} <- ask
  newBid' terms actor bidAmount

newBid' ::
  (MonadIO m, MonadFail m, MonadCardanoClient m, MonadTrace m) => AuctionTerms -> Actor -> Natural -> m ()
newBid' terms submitingActor bidAmount = do
  -- Actor is not neccesary bidder, on L2 it may be commiter
  logMsg "Doing new bid"

  (bidderAddress, bidderVk, bidderSk) <- addressAndKeysForActor submitingActor
  bidderMoneyUtxo <- queryUtxo (ByAddress bidderAddress)

  standingBidUtxo <- scriptUtxos StandingBid terms

  validateHasSingleUtxo standingBidUtxo "standingBidUtxo"
  validateHasSingleUtxo bidderMoneyUtxo "bidderMoneyUtxo"

  standingBidAddress <- scriptAddress StandingBid terms
  approvedBidders <- getApprovedBidders terms

  let txOutStandingBid bidderVk =
        TxOut
          (ShelleyAddressInEra standingBidAddress)
          valueStandingBid
          (mkInlineDatum datum)
          ReferenceScriptNone
        where
          mp = policy terms
          voucherCS = VoucherCS $ scriptCurrencySymbol mp
          datum =
            StandingBidDatum
              ( StandingBidState
                  { standingBid =
                      Just $
                        BidTerms
                          (toPlutusKeyHash $ verificationKeyHash bidderVk)
                          bidAmount
                  , approvedBidders = approvedBidders
                  }
              )
              voucherCS
          valueStandingBid =
            fromPlutusValue (assetClassValue (voucherAssetClass terms) 1)
              <> lovelaceToValue minLovelace
      standingBidWitness = mkInlinedDatumScriptWitness script NewBid
        where
          script = scriptPlutusScript StandingBid terms

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(bidderSk, bidderMoneyUtxo)]
        , additionalSigners = []
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

validateHasSingleUtxo :: MonadFail m => UTxO.UTxO -> String -> m ()
validateHasSingleUtxo utxo utxoName =
  when (length utxo /= 1) $
    fail $
      utxoName <> " UTxO has not exactly one TxIn"

cleanupTx :: AuctionTerms -> Runner ()
cleanupTx terms = do
  logMsg "Doing standing bid cleanup"

  (actorAddress, _, actorSk) <- addressAndKeys

  standingBidUtxo <- scriptUtxos StandingBid terms
  actorMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo

  -- FIXME: cover not proper UTxOs
  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(actorSk, actorMoneyUtxo)]
        , additionalSigners = []
        , referenceUtxo = mempty
        , witnessedUtxos =
            [ (standingBidWitness, standingBidUtxo)
            ]
        , collateral = Nothing
        , outs = []
        , toMint = toForgeStateToken terms BurnVoucher
        , changeAddress = actorAddress
        , validityBound = (Just $ cleanup terms, Nothing)
        }
  where
    standingBidWitness = mkInlinedDatumScriptWitness script Cleanup
      where
        script =
          fromPlutusScript @PlutusScriptV2 $
            getValidator $
              standingBidValidator terms
