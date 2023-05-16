module HydraAuction.Tx.Common (
  scriptUtxos,
  scriptAddress,
  scriptPlutusScript,
  createTwoMinAdaUtxo,
  currentAuctionStage,
  toForgeStateToken,
  scriptSingleUtxo,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad (void, when)
import Control.Monad.TimeMachine (MonadTime)
import Control.Monad.Trans (MonadIO)

-- Plutus imports
import PlutusLedgerApi.V1.Interval (member)

-- Cardano imports
import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (buildScriptAddress)
import Hydra.Cardano.Api (
  Address,
  BuildTx,
  CtxUTxO,
  PlutusScript,
  ShelleyAddr,
  TxIn,
  TxMintValue,
  TxOut,
  fromPlutusScript,
  lovelaceToValue,
  txOutValue,
  pattern PlutusScript,
  pattern ReferenceScriptNone,
  pattern ShelleyAddressInEra,
  pattern TxMintValueNone,
  pattern TxOut,
  pattern TxOutDatumNone,
 )
import Hydra.Chain.Direct.Util (isMarkedOutput)

-- Hydra auction imports

import HydraAuction.OnChain (
  AuctionScript (..),
  policy,
  scriptValidatorForTerms,
  singleUtxoScripts,
 )
import HydraAuction.OnChain.Common (stageToInterval)
import HydraAuction.OnChain.StateToken (
  StateTokenKind (..),
  stateTokenKindToTokenName,
 )
import HydraAuction.Types (
  AuctionStage,
  AuctionTerms,
  VoucherForgingRedeemer (..),
  auctionStages,
 )
import HydraAuctionUtils.Monads (
  MonadCardanoClient,
  MonadNetworkId (..),
  MonadQueryUtxo (..),
  MonadTrace,
  UtxoQuery (..),
 )
import HydraAuctionUtils.Monads.Actors (
  MonadHasActor,
  actorTipUtxo,
  addressAndKeys,
 )
import HydraAuctionUtils.Time (currentPlutusPOSIXTime)
import HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoSubmitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.Build (minLovelace, mintedTokens, tokenToAsset)
import HydraAuctionUtils.Tx.Utxo (
  filterAdaOnlyUtxo,
 )

currentAuctionStage ::
  (MonadTime timedMonad) => AuctionTerms -> timedMonad AuctionStage
currentAuctionStage terms = do
  currentTime <- currentPlutusPOSIXTime
  let matchingStages = filter (member currentTime . stageToInterval terms) auctionStages
  return $ case matchingStages of
    [stage] -> stage
    [_, stage] -> stage
    [] -> error "Impossible happend: no matching stages"
    _ -> error "Impossible happend: more than one matching stages"

toForgeStateToken :: AuctionTerms -> VoucherForgingRedeemer -> TxMintValue BuildTx
toForgeStateToken terms redeemer =
  mintedTokens
    (fromPlutusScript $ policy terms)
    redeemer
    [(tokenToAsset $ stateTokenKindToTokenName Voucher, num)]
  where
    num = case redeemer of
      MintVoucher -> 1
      BurnVoucher -> -1

createTwoMinAdaUtxo ::
  (MonadIO m, MonadCardanoClient m, MonadTrace m, MonadFail m, MonadHasActor m) =>
  m ((TxIn, TxOut CtxUTxO), (TxIn, TxOut CtxUTxO))
createTwoMinAdaUtxo = do
  (actorAddress, _, actorSk) <- addressAndKeys

  -- FIXUP: cover no money case, use single utxo
  actorMoneyUtxo <-
    UTxO.filter (not . isMarkedOutput) . filterAdaOnlyUtxo <$> actorTipUtxo

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(actorSk, actorMoneyUtxo)]
        , additionalSigners = []
        , referenceUtxo = mempty
        , witnessedUtxos = []
        , collateral = Nothing
        , outs = [minAdaOut actorAddress, minAdaOut actorAddress]
        , toMint = TxMintValueNone
        , changeAddress = actorAddress
        , validityBound = (Nothing, Nothing)
        }

  (utxo1 : utxo2 : _) <-
    UTxO.pairs . UTxO.filter (\x -> txOutValue x == lovelaceToValue minLovelace) . filterAdaOnlyUtxo <$> actorTipUtxo

  return (utxo1, utxo2)
  where
    minAdaOut actorAddress =
      TxOut
        (ShelleyAddressInEra actorAddress)
        (lovelaceToValue minLovelace)
        TxOutDatumNone
        ReferenceScriptNone

scriptPlutusScript :: AuctionScript -> AuctionTerms -> PlutusScript
scriptPlutusScript script terms = fromPlutusScript $ scriptValidatorForTerms script terms

scriptAddress :: MonadNetworkId m => AuctionScript -> AuctionTerms -> m (Address ShelleyAddr)
scriptAddress script terms =
  buildScriptAddress
    (PlutusScript $ scriptPlutusScript script terms)
    <$> askNetworkId

scriptUtxos :: (MonadNetworkId m, MonadQueryUtxo m) => AuctionScript -> AuctionTerms -> m UTxO.UTxO
scriptUtxos script terms = do
  scriptAddress' <- scriptAddress script terms
  queryUtxo (ByAddress scriptAddress')

scriptSingleUtxo ::
  (MonadNetworkId m, MonadQueryUtxo m, MonadFail m) =>
  AuctionScript ->
  AuctionTerms ->
  m (Maybe (TxIn, TxOut CtxUTxO))
scriptSingleUtxo script terms = do
  when (not $ script `elem` singleUtxoScripts) $
    fail $
      "Precondition failed: not single-utxo script: " <> show script
  utxos <- scriptUtxos script terms
  case UTxO.pairs utxos of
    [pair] -> return $ Just pair
    [] -> return Nothing
    _ ->
      fail $
        "Impossible happened: more than one UTxO for script " <> show script
