module HydraAuction.Tx.Deposit (
  mkDeposit,
  filterDepositGreaterThan,
  losingBidderClaimDeposit,
  sellerClaimDepositFor,
  parseBidDepositDatum,
  cleanupDeposit,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Plutus imports
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusTx.IsData.Class (fromData)

-- Cardano node imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports
import Hydra.Cardano.Api (
  Lovelace (..),
  TxOut,
  getScriptData,
  lovelaceToValue,
  toPlutusData,
  toPlutusKeyHash,
  txOutDatum,
  txOutValue,
  valueToLovelace,
  verificationKeyHash,
  pattern ReferenceScriptNone,
  pattern ShelleyAddressInEra,
  pattern TxMintValueNone,
  pattern TxOut,
  pattern TxOutDatumInline,
 )

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript (..), voucherCurrencySymbol)
import HydraAuction.OnChain.Common (stageToInterval)
import HydraAuction.Tx.Common (
  scriptAddress,
  scriptPlutusScript,
  scriptUtxos,
 )
import HydraAuction.Types (
  AuctionStage (..),
  AuctionTerms (..),
  BidDepositDatum (..),
  BidDepositRedeemer (..),
 )
import HydraAuctionUtils.L1.Runner (L1Runner)
import HydraAuctionUtils.Monads (
  logMsg,
 )
import HydraAuctionUtils.Monads.Actors (
  WithActorT,
  addressAndKeys,
 )
import HydraAuctionUtils.Plutus (extendIntervalRight)
import HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoSubmitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.Build (
  minLovelace,
  mkInlineDatum,
  mkInlinedDatumScriptWitness,
 )
import HydraAuctionUtils.Tx.Common (selectAdaUtxo)
import HydraAuctionUtils.Types.Natural (
  Natural,
  naturalToInt,
 )

parseBidDepositDatum :: TxOut ctx -> BidDepositDatum
parseBidDepositDatum out = case txOutDatum out of
  TxOutDatumInline scriptData ->
    case fromData $ toPlutusData $ getScriptData scriptData of
      Just bidDepositDatum -> bidDepositDatum
      Nothing ->
        error "Impossible happened: Cannot decode bid deposit datum"
  _ -> error "Impossible happened: No inline data for bid deposit"

filterDepositGreaterThan :: Natural -> UTxO.UTxO -> UTxO.UTxO
filterDepositGreaterThan minAmt =
  UTxO.filter
    ( \deposit -> case valueToLovelace (txOutValue deposit) of
        Just adaAmt -> adaAmt >= Lovelace (naturalToInt minAmt)
        Nothing -> False
    )

findDepositMatchingPubKeyHash :: AuctionTerms -> PubKeyHash -> UTxO.UTxO -> L1Runner UTxO.UTxO
findDepositMatchingPubKeyHash terms pkh allDeposits =
  case UTxO.find ((== expectedDatum) . parseBidDepositDatum) allDeposits of
    Nothing -> fail "Unable to find matching deposit"
    Just deposit -> pure $ UTxO.singleton deposit
  where
    voucherCS = voucherCurrencySymbol terms
    expectedDatum = BidDepositDatum pkh voucherCS

mkDeposit :: AuctionTerms -> Natural -> WithActorT L1Runner ()
mkDeposit terms depositAmount = do
  logMsg "Doing bidder deposit"

  depositAddress <- scriptAddress Deposit terms

  (bidderAddress, bidderVk, bidderSk) <- addressAndKeys

  let depositLovelace = Lovelace (naturalToInt depositAmount)
  bidderMoneyUtxo <- fromJust <$> selectAdaUtxo depositLovelace

  let voucherCS = voucherCurrencySymbol terms
      bidDepositDatum = BidDepositDatum (toPlutusKeyHash $ verificationKeyHash bidderVk) voucherCS
      bidDepositTxOut =
        TxOut
          (ShelleyAddressInEra depositAddress)
          depositValue
          (mkInlineDatum bidDepositDatum)
          ReferenceScriptNone
      depositValue = lovelaceToValue depositLovelace

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(bidderSk, bidderMoneyUtxo)]
        , additionalSigners = []
        , referenceUtxo = mempty
        , witnessedUtxos = []
        , collateral = Nothing
        , outs = [bidDepositTxOut]
        , toMint = TxMintValueNone
        , changeAddress = bidderAddress
        , validityBound = stageToInterval terms AnnouncedStage
        }

losingBidderClaimDeposit :: AuctionTerms -> WithActorT L1Runner ()
losingBidderClaimDeposit terms = do
  logMsg "Claiming bidder deposit"

  (bidderAddress, bidderVk, bidderSk) <- addressAndKeys

  bidderMoneyUtxo <- fromJust <$> selectAdaUtxo minLovelace
  standingBidUtxo <- scriptUtxos StandingBid terms
  allDeposits <- scriptUtxos Deposit terms

  deposit <- lift $ findDepositMatchingPubKeyHash terms (toPlutusKeyHash $ verificationKeyHash bidderVk) allDeposits

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(bidderSk, bidderMoneyUtxo)]
        , additionalSigners = []
        , referenceUtxo = standingBidUtxo
        , witnessedUtxos = [(depositWitness, deposit)]
        , collateral = Nothing
        , outs = []
        , toMint = TxMintValueNone
        , changeAddress = bidderAddress
        , validityBound = extendIntervalRight $ stageToInterval terms BiddingEndedStage
        }
  where
    depositScript = scriptPlutusScript Deposit terms
    depositWitness = mkInlinedDatumScriptWitness depositScript LosingBidder

sellerClaimDepositFor :: AuctionTerms -> PubKeyHash -> WithActorT L1Runner ()
sellerClaimDepositFor terms bidderPkh = do
  logMsg "Seller claiming bidder deposit"

  (sellerAddress, _, sellerSk) <- addressAndKeys

  sellerMoneyUtxo <- fromJust <$> selectAdaUtxo minLovelace
  standingBidUtxo <- scriptUtxos StandingBid terms
  auctionEscrowUtxo <- scriptUtxos Escrow terms

  allDeposits <- scriptUtxos Deposit terms

  deposit <- lift $ findDepositMatchingPubKeyHash terms bidderPkh allDeposits

  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(sellerSk, sellerMoneyUtxo)]
        , additionalSigners = []
        , referenceUtxo = standingBidUtxo <> auctionEscrowUtxo
        , witnessedUtxos = [(depositWitness, deposit)]
        , collateral = Nothing
        , outs = []
        , toMint = TxMintValueNone
        , changeAddress = sellerAddress
        , validityBound = extendIntervalRight $ stageToInterval terms VoucherExpiredStage
        }
  where
    depositScript = scriptPlutusScript Deposit terms
    depositWitness = mkInlinedDatumScriptWitness depositScript SellerClaimsDeposit

cleanupDeposit :: AuctionTerms -> WithActorT L1Runner ()
cleanupDeposit terms = do
  logMsg "Cleanup bidder deposit"

  (bidderAddress, bidderVk, bidderSk) <- addressAndKeys

  bidderMoneyUtxo <- fromJust <$> selectAdaUtxo minLovelace

  allDeposits <- scriptUtxos Deposit terms

  deposit <-
    lift $
      findDepositMatchingPubKeyHash terms (toPlutusKeyHash $ verificationKeyHash bidderVk) allDeposits
  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(bidderSk, bidderMoneyUtxo)]
        , additionalSigners = []
        , referenceUtxo = mempty
        , witnessedUtxos = [(depositWitness, deposit)]
        , collateral = Nothing
        , outs = []
        , toMint = TxMintValueNone
        , changeAddress = bidderAddress
        , validityBound = stageToInterval terms CleanupStage
        }
  where
    depositScript = scriptPlutusScript Deposit terms
    depositWitness = mkInlinedDatumScriptWitness depositScript CleanupDeposit
