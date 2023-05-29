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
  sellerSignatureForActor,
  NewBidTxInfo (..),
) where

-- Prelude imports
import Hydra.Prelude (MonadIO, rightToMaybe, void)
import Prelude

-- Haskell imports

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask))
import Crypto.Sign.Ed25519 (SecretKey (..), Signature (..), dsign)
import Data.Maybe (listToMaybe, mapMaybe)

-- Plutus imports

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value (assetClassValue)
import PlutusLedgerApi.V2 (BuiltinByteString, FromData, PubKeyHash, fromBuiltin, fromData, toBuiltin)
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
  serialiseToRawBytes,
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
  voucherAssetClass,
  voucherCurrencySymbol,
 )
import HydraAuction.OnChain.StandingBid (
  bidderSignatureMessage,
  sellerSignatureMessage,
 )
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
  AuctionTerms (..),
  BidTerms (..),
  StandingBidDatum (..),
  StandingBidRedeemer (..),
  StandingBidState (..),
  VoucherForgingRedeemer (BurnVoucher),
 )
import HydraAuctionUtils.Extras.Plutus (scriptCurrencySymbol)
import HydraAuctionUtils.Fixture (Actor, actorFromPkh, getActorPubKeyHash, keysFor)
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
        (Just (BidTerms {bidderPKH})) -> Just bidderPKH
        Nothing -> Nothing
    Nothing -> Nothing

newBid :: AuctionTerms -> Natural -> BuiltinByteString -> L1Runner ()
newBid terms bidAmount sellerSignature = do
  actor <- askActor
  (_, _, submitterSk) <- addressAndKeysForActor actor
  let datum = createStandingBidDatum terms bidAmount sellerSignature submitterSk

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

sellerSignatureForActor :: AuctionTerms -> Actor -> IO BuiltinByteString
sellerSignatureForActor terms actor = do
  sellerActor <- actorFromPkh (sellerPKH terms)
  (sellerVk, sellerSk) <- keysFor sellerActor
  (actorVK, _) <- keysFor actor
  actorPKH <- getActorPubKeyHash actor
  -- dsign expects keys to be 64bytes long and to be the
  -- concatenation of an skey and vkey
  let sellerSecretKey = SecretKey $ serialiseToRawBytes sellerSk <> serialiseToRawBytes sellerVk
      sellerMessge = fromBuiltin $ sellerSignatureMessage (voucherCurrencySymbol terms) (toBuiltin $ serialiseToRawBytes actorVK) actorPKH
      Signature sellerSignature = dsign sellerSecretKey sellerMessge
  pure $ toBuiltin sellerSignature

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
      , validityBound = (Just $ biddingStart terms, Just $ biddingEnd terms)
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
  BuiltinByteString ->
  SigningKey PaymentKey ->
  StandingBidDatum
createStandingBidDatum terms bidAmount sellerSignature bidderSk =
  StandingBidDatum
    ( StandingBidState
        { standingBid =
            Just $
              BidTerms
                { bidderPKH
                , bidderVK = toBuiltin $ serialiseToRawBytes derivedVK
                , bidAmount
                , bidderSignature = toBuiltin bidderSignature
                , sellerSignature
                }
        }
    )
    voucherCS
  where
    auctionId = voucherCurrencySymbol terms
    derivedVK = getVerificationKey bidderSk
    bidderPKH = toPlutusKeyHash $ verificationKeyHash derivedVK
    bidderSecretKey = SecretKey $ serialiseToRawBytes bidderSk <> serialiseToRawBytes derivedVK
    bidderMessage = fromBuiltin $ bidderSignatureMessage auctionId bidAmount bidderPKH
    Signature bidderSignature = dsign bidderSecretKey bidderMessage
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
        , validityBound = (Just $ cleanup terms, Nothing)
        }
  where
    standingBidWitness = mkInlinedDatumScriptWitness script Cleanup
      where
        script =
          fromPlutusScript @PlutusScriptV2 $
            standingBidValidator terms
