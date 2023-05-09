module HydraAuction.Tx.StandingBid (
  newBid,
  cleanupTx,
  decodeInlineDatum,
  queryStandingBidDatum,
  currentWinningBidder,
  createNewBidTx,
  createStandingBidDatum,
  moveToHydra,
  sellerSignatureForActor,
) where

-- Prelude imports
import Hydra.Prelude (MonadIO, rightToMaybe, void)
import Prelude

-- Haskell imports
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask))
import Crypto.Sign.Ed25519 (SecretKey (..), Signature (..), dsign)

-- Plutus imports
import Plutus.V1.Ledger.Value (assetClassValue)
import Plutus.V2.Ledger.Api (BuiltinByteString, FromData, PubKeyHash, fromBuiltin, fromData, getValidator, toBuiltin)

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
  serialiseToRawBytes,
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
import HydraAuction.OnChain.StandingBid (
  bidderSignatureMessage,
  sellerSignatureMessage,
 )
import HydraAuctionUtils.Monads.Actors (
  actorTipUtxo,
  addressAndKeys,
 )

import HydraAuction.Tx.Common (
  scriptAddress,
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
import HydraAuctionUtils.Tx.Build (
  minLovelace,
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
      case fromData $ toPlutusData scriptData of
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

newBid :: AuctionTerms -> Natural -> L1Runner ()
newBid terms bidAmount = do
  MkExecutionContext {actor} <- ask
  (_, _, submitterSk) <- addressAndKeysForActor actor
  sellerSignature <- liftIO $ sellerSignatureForActor terms actor
  let datum = createStandingBidDatum terms bidAmount sellerSignature submitterSk
  tx <- createNewBidTx terms actor datum
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
      sellerMessge = fromBuiltin $ sellerSignatureMessage (hydraHeadId terms) (toBuiltin $ serialiseToRawBytes actorVK) actorPKH
      Signature sellerSignature = dsign sellerSecretKey sellerMessge
  pure $ toBuiltin sellerSignature

createNewBidTx ::
  (MonadIO m, MonadFail m, MonadCardanoClient m, MonadTrace m) =>
  AuctionTerms ->
  Actor ->
  StandingBidDatum ->
  m Tx
createNewBidTx terms submitingActor bidDatum = do
  -- Actor is not neccesary bidder, on L2 it may be commiter
  logMsg "Creating new bid transaction"

  (submitterAddress, _, submitterSk) <- addressAndKeysForActor submitingActor
  submitterMoneyUtxo <- queryUtxo (ByAddress submitterAddress)
  validateHasSingleUtxo submitterMoneyUtxo "submitterMoneyUtxo"

  mStandingBidUtxo <- scriptSingleUtxo StandingBid terms
  standingBidSingleUtxo <- case mStandingBidUtxo of
    Just x -> return x
    Nothing -> fail "Standing bid cannot be found"

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
              <> lovelaceToValue (2 * minLovelace)
      standingBidWitness = mkInlinedDatumScriptWitness script NewBid
        where
          script = scriptPlutusScript StandingBid terms

  autoCreateTx $
    AutoCreateParams
      { signedUtxos = [(submitterSk, submitterMoneyUtxo)]
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
    auctionId = hydraHeadId terms
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
            getValidator $
              standingBidValidator terms
