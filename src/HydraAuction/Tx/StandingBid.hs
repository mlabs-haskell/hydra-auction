module HydraAuction.Tx.StandingBid (
  newBid,
  cleanupTx,
  decodeInlineDatum,
  queryStandingBidDatum,
  currentWinningBidder,
  createNewBidTx,
  createStandingBidDatum,
  decodeNewBidTxOnL2,
  moveToHydra,
  NewBidTxInfo (..),
) where

-- Prelude imports
import Hydra.Prelude (MonadIO, rightToMaybe, void)
import Prelude

-- Haskell imports

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask))
import Data.Maybe (listToMaybe, mapMaybe)

-- Plutus imports

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusTx.IsData.Class (FromData, fromData)

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
  fromScriptData,
  getScriptData,
  getTxBody,
  lovelaceToValue,
  toPlutusData,
  toPlutusKeyHash,
  txExtraKeyWits,
  txIns,
  txOutDatum,
  txOuts,
  verificationKeyHash,
  pattern KeyWitness,
  pattern ReferenceScriptNone,
  pattern ScriptWitness,
  pattern ShelleyAddressInEra,
  pattern TxBody,
  pattern TxBodyContent,
  pattern TxExtraKeyWitnesses,
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
 )
import HydraAuction.OnChain.Common (stageToInterval)
import HydraAuctionUtils.Monads.Actors (
  actorTipUtxo,
  addressAndKeys,
  askActor,
 )

import HydraAuction.Tx.Common (
  createTwoMinAdaUtxo,
  scriptPlutusScript,
  scriptSingleUtxo,
  scriptUtxos,
  toForgeStateToken,
 )
import HydraAuction.Types (
  ApprovedBidders (..),
  AuctionStage (..),
  AuctionTerms (..),
  BidTerms (..),
  StandingBidDatum (..),
  StandingBidRedeemer (..),
  StandingBidState (..),
  VoucherForgingRedeemer (BurnVoucher),
 )
import HydraAuctionUtils.Extras.Plutus (scriptCurrencySymbol)
import HydraAuctionUtils.Fixture (Actor)
import HydraAuctionUtils.L1.Runner (ExecutionContext (..), L1Runner)
import HydraAuctionUtils.Monads (
  MonadCardanoClient,
  MonadNetworkId,
  MonadQueryUtxo (..),
  MonadTrace,
  addressAndKeysForActor,
  logMsg,
  submitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoCreateTx,
  autoSubmitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.Build (
  mkInlineDatum,
  mkInlinedDatumScriptWitness,
 )
import HydraAuctionUtils.Tx.Utxo (
  filterAdaOnlyUtxo,
 )
import HydraAuctionUtils.Types.Natural (Natural)

data DatumDecodingError = CannotDecodeDatum | NoInlineDatum

-- FIXME: move to utils
decodeInlineDatum ::
  forall a. FromData a => TxOut CtxUTxO -> Either DatumDecodingError a
decodeInlineDatum out =
  case txOutDatum out of
    TxOutDatumInline scriptData ->
      case fromData $ toPlutusData $ getScriptData scriptData of
        Just standingBidDatum -> Right standingBidDatum
        Nothing -> Left CannotDecodeDatum
    _ -> Left NoInlineDatum

queryStandingBidDatum ::
  (MonadNetworkId m, MonadQueryUtxo m, MonadFail m) =>
  AuctionTerms ->
  m (Maybe StandingBidDatum)
queryStandingBidDatum terms = do
  mStandingBidUtxo <- scriptSingleUtxo StandingBid terms
  return $ case mStandingBidUtxo of
    Just (_, txOut) -> rightToMaybe $ decodeInlineDatum txOut
    Nothing -> Nothing

currentWinningBidder ::
  (MonadNetworkId m, MonadQueryUtxo m, MonadFail m) => AuctionTerms -> m (Maybe PubKeyHash)
currentWinningBidder terms = do
  mDatum <- queryStandingBidDatum terms
  return $ case mDatum of
    Just (StandingBidDatum {standingBidState}) ->
      case standingBid standingBidState of
        (Just (BidTerms {bidBidder})) -> Just bidBidder
        Nothing -> Nothing
    Nothing -> Nothing

newBid :: AuctionTerms -> Natural -> L1Runner ()
newBid terms bidAmount = do
  actor <- askActor
  (_, submitterVk, _) <- addressAndKeys
  let datum = createStandingBidDatum terms bidAmount submitterVk

  mStandingBidUtxo <- scriptSingleUtxo StandingBid terms
  standingBidSingleUtxo <- case mStandingBidUtxo of
    Just x -> return x
    Nothing -> fail "Standing bid cannot be found"

  moneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo
  submitterMoneyUtxo <- case listToMaybe $ UTxO.pairs moneyUtxo of
    Just x -> return x
    Nothing -> fail "Submiter does not have money for transaction"

  tx <-
    createNewBidTx
      terms
      actor
      standingBidSingleUtxo
      submitterMoneyUtxo
      datum
  submitAndAwaitTx tx

createNewBidTx ::
  ( MonadIO m
  , MonadFail m
  , MonadCardanoClient m
  , MonadTrace m
  ) =>
  AuctionTerms ->
  Actor ->
  (TxIn, TxOut CtxUTxO) ->
  (TxIn, TxOut CtxUTxO) ->
  StandingBidDatum ->
  m Tx
createNewBidTx terms actor standingBidSingleUtxo submitterMoneyUtxo bidDatum = do
  -- Actor is not neccesary bidder, on L2 it may be commiter
  logMsg "Creating new bid transaction"

  (submitterAddress, _, submitterSk) <- addressAndKeysForActor actor

  autoCreateTx $
    AutoCreateParams
      { signedUtxos = [(submitterSk, UTxO.fromPairs [submitterMoneyUtxo])]
      , additionalSigners = []
      , referenceUtxo = mempty
      , witnessedUtxos =
          [ (standingBidWitness, UTxO.fromPairs [standingBidSingleUtxo])
          ]
      , collateral = Nothing
      , outs = [txOutStandingBid]
      , toMint = TxMintValueNone
      , changeAddress = submitterAddress
      , validityBound = stageToInterval terms BiddingStartedStage
      }
  where
    TxOut (ShelleyAddressInEra standingBidAddress) valueStandingBid _ _ =
      snd standingBidSingleUtxo
    txOutStandingBid =
      TxOut
        (ShelleyAddressInEra standingBidAddress)
        valueStandingBid
        (mkInlineDatum bidDatum)
        ReferenceScriptNone
    standingBidWitness = mkInlinedDatumScriptWitness script NewBid
      where
        script = scriptPlutusScript StandingBid terms

data NewBidTxInfo = MkNewBidTxInfo
  { submitterPKH :: PubKeyHash
  , isStandingBidInvalidated :: Bool
  -- ^ Means that StandingBid used as input is no longer alive
  , newBidDatum :: StandingBidDatum
  }

decodeNewBidTxOnL2 :: Tx -> UTxO.UTxO -> Maybe NewBidTxInfo
decodeNewBidTxOnL2 tx currentUtxo = do
  [newBidDatum] <- return $ mapMaybe parseStandingBidTxOut txOuts
  TxExtraKeyWitnesses [submitterPKH'] <- return txExtraKeyWits
  return $
    MkNewBidTxInfo
      { submitterPKH = toPlutusKeyHash submitterPKH'
      , isStandingBidInvalidated
      , newBidDatum
      }
  where
    TxBody TxBodyContent {txIns, txOuts, txExtraKeyWits} = getTxBody tx
    resolveInput input = UTxO.resolve input currentUtxo
    resolvedInputs = mapMaybe (resolveInput . fst) txIns
    parseStandingBidTxOut (TxOut _ _ (TxOutDatumInline datum) _) =
      fromScriptData datum :: Maybe StandingBidDatum
    parseStandingBidTxOut _ = Nothing
    -- Standing bid is already invalidated if it is not in current utxo
    isStandingBidInvalidated =
      null $ mapMaybe parseStandingBidTxOut resolvedInputs

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
  L1Runner ()
moveToHydra headId terms (standingBidTxIn, standingBidTxOut) = do
  -- FIXME: get headId from AuctionTerms
  -- FIXME: should use already deployed registry
  (_, scriptRegistry) <- do
    MkExecutionContext {node} <- ask
    liftIO $ prepareScriptRegistry node

  (utxo1, utxo2) <- createTwoMinAdaUtxo

  void $
    submitAndAwaitCommitTx
      scriptRegistry
      headId
      utxo1
      (UTxO.fromPairs [utxo2])
      (standingBidTxIn, standingBidTxOut, standingBidWitness)
  where
    script =
      fromPlutusScript @PlutusScriptV2 $
        standingBidValidator terms
    standingBidWitness = mkInlinedDatumScriptWitness script MoveToHydra

cleanupTx :: AuctionTerms -> L1Runner ()
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
        , validityBound = stageToInterval terms CleanupStage
        }
  where
    standingBidWitness = mkInlinedDatumScriptWitness script Cleanup
      where
        script =
          fromPlutusScript @PlutusScriptV2 $
            standingBidValidator terms
