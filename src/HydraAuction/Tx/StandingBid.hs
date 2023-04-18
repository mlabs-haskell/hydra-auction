module HydraAuction.Tx.StandingBid (
  newBid,
  cleanupTx,
  decodeInlineDatum,
  queryStandingBidDatum,
  currentWinningBidder,
  newBid',
  createStandingBidDatum,
  moveToHydra,
) where

-- Prelude imports
-- Prelude imports
import Hydra.Prelude (MonadIO, rightToMaybe, void)
import Prelude

-- Haskell imports
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask))

-- Plutus imports
import Plutus.V1.Ledger.Value (assetClassValue)
import Plutus.V2.Ledger.Api (FromData, PubKeyHash, fromData, getValidator)

-- Hydra imports
import Cardano.Api.UTxO qualified as UTxO
import Hydra.Cardano.Api (
  CtxUTxO,
  Key (..),
  PaymentKey,
  PlutusScriptV2,
  Tx,
  TxIn,
  TxOut,
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
import Hydra.Chain (HeadId)

-- Hydra auction imports

import HydraAuction.Addresses (VoucherCS (..))
import HydraAuction.HydraHacks (prepareScriptRegistry, submitAndAwaitCommitTx)
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
  ApprovedBidders (..),
  AuctionTerms (..),
  BidTerms (..),
  Natural,
  StandingBidDatum (..),
  StandingBidRedeemer (..),
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
  submitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoCreateTx,
  autoSubmitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.Utxo (
  filterAdaOnlyUtxo,
 )

data DatumDecodingError = CannotDecodeDatum | NoInlineDatum

-- FIXME: move to utils
decodeInlineDatum ::
  forall a. FromData a => TxOut CtxUTxO -> Either DatumDecodingError a
decodeInlineDatum out =
  case txOutDatum out of
    TxOutDatumInline scriptData ->
      case fromData $ toPlutusData scriptData of
        Just standingBidDatum -> Right standingBidDatum
        Nothing -> Left CannotDecodeDatum
    _ -> Left NoInlineDatum

queryStandingBidDatum ::
  (MonadNetworkId m, MonadQueryUtxo m) =>
  AuctionTerms ->
  m (Maybe StandingBidDatum)
queryStandingBidDatum terms = do
  standingBidUtxo <- scriptUtxos StandingBid terms
  return $ case UTxO.pairs standingBidUtxo of
    [] -> Nothing
    [(_, txOut)] -> rightToMaybe $ decodeInlineDatum txOut
    _ -> error "Impossible happened: more than one StandingBid exists"

currentWinningBidder :: (MonadNetworkId m, MonadQueryUtxo m) => AuctionTerms -> m (Maybe PubKeyHash)
currentWinningBidder terms = do
  mDatum <- queryStandingBidDatum terms
  return $ case mDatum of
    Just (StandingBidDatum {standingBidState}) ->
      case standingBid standingBidState of
        (Just (BidTerms {bidBidder})) -> Just bidBidder
        Nothing -> Nothing
    Nothing -> Nothing

newBid :: AuctionTerms -> Natural -> Runner ()
newBid terms bidAmount = do
  MkExecutionContext {actor} <- ask
  (_, submitterVk, _) <- addressAndKeysForActor actor
  -- FIXME: reflect bidder vs submitter missmatch in API
  let datum = createStandingBidDatum terms bidAmount submitterVk
  tx <- newBid' terms actor datum
  submitAndAwaitTx tx

newBid' ::
  (MonadIO m, MonadFail m, MonadCardanoClient m, MonadTrace m) =>
  AuctionTerms ->
  Actor ->
  StandingBidDatum ->
  m Tx
newBid' terms submitingActor bidDatum = do
  -- Actor is not neccesary bidder, on L2 it may be commiter
  logMsg "Doing new bid"

  (submitterAddress, _, submitterSk) <- addressAndKeysForActor submitingActor
  submitterMoneyUtxo <- queryUtxo (ByAddress submitterAddress)

  standingBidUtxo <- scriptUtxos StandingBid terms

  validateHasSingleUtxo standingBidUtxo "standingBidUtxo"
  validateHasSingleUtxo submitterMoneyUtxo "bidderMoneyUtxo"

  standingBidAddress <- scriptAddress StandingBid terms

  let txOutStandingBid =
        TxOut
          (ShelleyAddressInEra standingBidAddress)
          valueStandingBid
          (mkInlineDatum bidDatum)
          ReferenceScriptNone
        where
          valueStandingBid =
            fromPlutusValue (assetClassValue (voucherAssetClass terms) 1)
              <> lovelaceToValue minLovelace
      standingBidWitness = mkInlinedDatumScriptWitness script NewBid
        where
          script = scriptPlutusScript StandingBid terms

  autoCreateTx $
    AutoCreateParams
      { signedUtxos = [(submitterSk, submitterMoneyUtxo)]
      , additionalSigners = []
      , referenceUtxo = mempty
      , witnessedUtxos =
          [ (standingBidWitness, standingBidUtxo)
          ]
      , collateral = Nothing
      , outs = [txOutStandingBid]
      , toMint = TxMintValueNone
      , changeAddress = submitterAddress
      , validityBound = (Just $ biddingStart terms, Just $ biddingEnd terms)
      }

createStandingBidDatum ::
  AuctionTerms ->
  Natural ->
  VerificationKey PaymentKey ->
  StandingBidDatum
createStandingBidDatum terms bidAmount bidderVk =
  -- FIXME: approved bidders disabled until M6
  StandingBidDatum
    ( StandingBidState
        { standingBid =
            Just $
              BidTerms
                (toPlutusKeyHash $ verificationKeyHash bidderVk)
                bidAmount
        , approvedBidders = emptyBidders
        }
    )
    voucherCS
  where
    emptyBidders = ApprovedBidders []
    mp = policy terms
    voucherCS = VoucherCS $ scriptCurrencySymbol mp

moveToHydra ::
  HeadId ->
  AuctionTerms ->
  (TxIn, TxOut CtxUTxO) ->
  Runner ()
moveToHydra headId terms (standingBidTxIn, standingBidTxOut) = do
  -- FIXME: get headId from AuctionTerms
  -- FIXME: should use already deployed registry
  scriptRegistry <- do
    MkExecutionContext {node} <- ask
    liftIO $ prepareScriptRegistry node

  void $
    submitAndAwaitCommitTx
      scriptRegistry
      headId
      (standingBidTxIn, standingBidTxOut, standingBidWitness)
  where
    script =
      fromPlutusScript @PlutusScriptV2 $
        getValidator $
          standingBidValidator terms
    standingBidWitness = mkInlinedDatumScriptWitness script MoveToHydra

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
