module HydraAuction.Tx.Common (
  scriptUtxos,
  scriptAddress,
  scriptPlutusScript,
  createMinAdaUtxo,
  currentAuctionStage,
  toForgeStateToken,
  scriptSingleUtxo,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports
import Control.Monad.TimeMachine (MonadTime)

-- Plutus imports
import PlutusLedgerApi.V1.Interval (member)
import PlutusLedgerApi.V2 (always)

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
  addressAndKeys,
 )
import HydraAuctionUtils.Time (currentPlutusPOSIXTime)
import HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoSubmitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.Build (minLovelace, mintedTokens, tokenToAsset)
import HydraAuctionUtils.Tx.Common (actorAdaOnlyUtxo, selectAdaUtxo)

currentAuctionStage ::
  (MonadTime timedMonad) => AuctionTerms -> timedMonad AuctionStage
currentAuctionStage terms = do
  currentTime <- currentPlutusPOSIXTime
  let matchingStages = filter (member currentTime . stageToInterval terms) auctionStages
  return $ case matchingStages of
    [stage] -> stage
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

createMinAdaUtxo ::
  (MonadIO m, MonadCardanoClient m, MonadTrace m, MonadFail m, MonadHasActor m) =>
  m (TxIn, TxOut CtxUTxO)
createMinAdaUtxo = do
  (actorAddress, _, actorSk) <- addressAndKeys
  actorMoneyUtxo <- fromJust <$> selectAdaUtxo minLovelace

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(actorSk, actorMoneyUtxo)]
        , additionalSigners = []
        , referenceUtxo = mempty
        , witnessedUtxos = []
        , collateral = Nothing
        , outs = [minAdaOut actorAddress]
        , toMint = TxMintValueNone
        , changeAddress = actorAddress
        , validityBound = always
        }

  let filterMinLovelace = UTxO.filter $
        \x -> txOutValue x == lovelaceToValue minLovelace
  utxo : _ <- UTxO.pairs . filterMinLovelace <$> actorAdaOnlyUtxo
  return utxo
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
