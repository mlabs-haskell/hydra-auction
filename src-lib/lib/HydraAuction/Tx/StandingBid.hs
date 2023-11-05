module HydraAuction.Tx.StandingBid (
  newBid,
  cleanupTx,
  decodeInlineDatum,
  queryStandingBidDatum,
  currentWinningBidder,
  createNewBidTx,
  createStandingBidDatum,
  decodeNewBidTxOnL2,
  moveToHydraTx,
  sellerSignatureForActor,
  NewBidTxInfo (..),
) where

-- Prelude imports
import Hydra.Prelude (rightToMaybe)
import HydraAuctionUtils.Prelude

-- Haskell imports

import Crypto.Sign.Ed25519 (SecretKey (..), Signature (..), dsign)
import Data.Maybe (listToMaybe, mapMaybe)

-- Plutus imports

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (BuiltinByteString, fromBuiltin, toBuiltin)

-- Hydra imports

import Cardano.Api.UTxO qualified as UTxO
import Hydra.API.HTTPServer (
  DraftCommitTxRequest (..),
  ScriptInfo (..),
  TxOutWithWitness (..),
 )
import Hydra.Cardano.Api (
  -- CtxTx,
  CtxUTxO,
  Key (..),
  Lovelace (..),
  PaymentKey,
  PlutusScriptV2,
  Tx,
  TxIn,
  TxOut,
  fromPlutusScript,
  fromScriptData,
  getTxBody,
  serialiseToRawBytes,
  toPlutusKeyHash,
  toScriptData,
  txExtraKeyWits,
  txIns,
  -- txOutScriptData,
  txOuts,
  verificationKeyHash,
  pattern ReferenceScriptNone,
  pattern TxBody,
  pattern TxExtraKeyWitnesses,
  pattern TxMintValueNone,
  pattern TxOut,
  pattern TxOutDatumInline,
 )
import Hydra.Chain (HeadId)

import HydraAuction.OnChain (
  AuctionScript (StandingBid),
  standingBidValidator,
  voucherCurrencySymbol,
 )
import HydraAuction.OnChain.Common (stageToInterval)
import HydraAuction.OnChain.StandingBid (
  bidderSignatureMessage,
  sellerSignatureMessage,
 )
import HydraAuctionUtils.Monads.Actors (
  WithActorT,
  addressAndKeys,
  askActor,
  withActor,
 )
import HydraAuctionUtils.Tx.Utxo (decodeInlineDatum)

import HydraAuction.Tx.Common (
  scriptPlutusScript,
  scriptSingleUtxo,
  scriptUtxos,
  toForgeStateToken,
 )
import HydraAuction.Types (
  AuctionStage (..),
  AuctionTerms (..),
  BidTerms (..),
  StandingBidDatum (..),
  StandingBidRedeemer (..),
  StandingBidState (..),
  VoucherForgingRedeemer (BurnVoucher),
 )
import HydraAuctionUtils.Fixture (Actor (..), actorFromPkh, getActorPubKeyHash, keysFor)
import HydraAuctionUtils.Hydra.Monad (MonadHydra (..))
-- import HydraAuctionUtils.Hydra.Runner (runL1RunnerInComposite)
import HydraAuctionUtils.L1.Runner (L1Runner)
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
  minLovelace,
  mkInlineDatum,
  mkInlinedDatumScriptWitness,
 )
import HydraAuctionUtils.Tx.Common (
  createMinAdaUtxo,
  selectAdaUtxo,
 )
import HydraAuctionUtils.Types.Natural (Natural, naturalToInt)

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

newBid :: AuctionTerms -> Natural -> BuiltinByteString -> WithActorT L1Runner ()
newBid terms bidAmount sellerSignature = do
  actor <- askActor
  (_, _, submitterSk) <- addressAndKeysForActor actor
  let datum = createStandingBidDatum terms bidAmount sellerSignature submitterSk

  mStandingBidUtxo <- scriptSingleUtxo StandingBid terms
  standingBidSingleUtxo <- case mStandingBidUtxo of
    Just x -> return x
    Nothing -> fail "Standing bid cannot be found"

  moneyUtxo <- fromJust <$> selectAdaUtxo (Lovelace $ naturalToInt bidAmount)
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
  void $ submitAndAwaitTx tx

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
      , validityBound = stageToInterval terms BiddingStartedStage
      }
  where
    TxOut standingBidAddress valueStandingBid _ _ =
      snd standingBidSingleUtxo
    txOutStandingBid =
      TxOut
        standingBidAddress
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
  [newBidDatum] <-
    return $ mapMaybe parseStandingBidTxOut $ txOuts bodyContent
  TxExtraKeyWitnesses [submitterPKH'] <- return $ txExtraKeyWits bodyContent
  return $
    MkNewBidTxInfo
      { submitterPKH = toPlutusKeyHash submitterPKH'
      , isStandingBidInvalidated
      , newBidDatum
      }
  where
    TxBody bodyContent = getTxBody tx
    resolveInput input = UTxO.resolve input currentUtxo
    resolvedInputs = mapMaybe (resolveInput . fst) $ txIns bodyContent
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
    voucherCS = voucherCurrencySymbol terms
    derivedVK = getVerificationKey bidderSk
    bidderPKH = toPlutusKeyHash $ verificationKeyHash derivedVK
    bidderSecretKey = SecretKey $ serialiseToRawBytes bidderSk <> serialiseToRawBytes derivedVK
    bidderMessage = fromBuiltin $ bidderSignatureMessage voucherCS bidAmount bidderPKH
    Signature bidderSignature = dsign bidderSecretKey bidderMessage

{- | Uses or creates minAda Utxo for collateral
 FIXME: remove? is simple now
-}
moveToHydraTx ::
  (HasCallStack, MonadHydra m, MonadIO m) =>
  HeadId ->
  AuctionTerms ->
  (TxIn, TxOut CtxUTxO) ->
  m Tx
moveToHydraTx _ terms (standingBidTxIn, standingBidTxOut) = do
  (collateralTxIn, collateralTxOut) <- runL1RunnerInComposite $ withActor Faucet createMinAdaUtxo
  createCommitTx $
    DraftCommitTxRequest $
      UTxO.fromPairs
        [
          ( standingBidTxIn
          , TxOutWithWitness standingBidTxOut info
          )
        , (collateralTxIn, TxOutWithWitness collateralTxOut Nothing)
        ]
  where
    script =
      fromPlutusScript @PlutusScriptV2 $
        standingBidValidator terms
    info =
      Just $
        ScriptInfo
          { redeemer = toScriptData MoveToHydra
          , datum = Nothing
          , plutusV2Script = script
          }

cleanupTx :: AuctionTerms -> WithActorT L1Runner ()
cleanupTx terms = do
  logMsg "Doing standing bid cleanup"

  (actorAddress, _, actorSk) <- addressAndKeys

  standingBidUtxo <- scriptUtxos StandingBid terms
  actorMoneyUtxo <- fromJust <$> selectAdaUtxo minLovelace

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
