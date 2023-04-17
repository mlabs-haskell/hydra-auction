module HydraAuction.Tx.Deposit (
  mkDeposit,
  filterDepositGreaterThan,
  losingBidderClaimDeposit,
  sellerClaimDepositFor,
  parseBidDepositDatum,
  cleanupDeposit,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad (void)

-- Plutus imports
import Plutus.V2.Ledger.Api (PubKeyHash, fromData)

-- Cardano node imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports
import Hydra.Cardano.Api (
  Lovelace (..),
  TxOut,
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
import HydraAuction.Addresses (VoucherCS (..))
import HydraAuction.OnChain (AuctionScript (..), policy)
import HydraAuction.Runner (Runner)
import HydraAuction.Tx.Common (
  actorTipUtxo,
  addressAndKeys,
  mkInlineDatum,
  mkInlinedDatumScriptWitness,
  scriptAddress,
  scriptPlutusScript,
  scriptUtxos,
 )
import HydraAuction.Types (
  AuctionTerms (..),
  BidDepositDatum (..),
  BidDepositRedeemer (..),
  Natural,
  naturalToInt,
 )
import HydraAuctionUtils.Extras.Plutus (scriptCurrencySymbol)
import HydraAuctionUtils.Monads (
  logMsg,
 )
import HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoSubmitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.Utxo (
  filterAdaOnlyUtxo,
 )

parseBidDepositDatum :: TxOut ctx -> BidDepositDatum
parseBidDepositDatum out = case txOutDatum out of
  TxOutDatumInline scriptData ->
    case fromData $ toPlutusData scriptData of
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

findDepositMatchingPubKeyHash :: AuctionTerms -> PubKeyHash -> UTxO.UTxO -> Runner UTxO.UTxO
findDepositMatchingPubKeyHash terms pkh allDeposits =
  case UTxO.find ((== expectedDatum) . parseBidDepositDatum) allDeposits of
    Nothing -> fail "Unable to find matching deposit"
    Just deposit -> pure $ UTxO.singleton deposit
  where
    mp = policy terms
    voucherCS = VoucherCS $ scriptCurrencySymbol mp
    expectedDatum = BidDepositDatum pkh voucherCS

mkDeposit :: AuctionTerms -> Natural -> Runner ()
mkDeposit terms depositAmount = do
  logMsg "Doing bidder deposit"

  depositAddress <- scriptAddress Deposit terms

  (bidderAddress, bidderVk, bidderSk) <- addressAndKeys

  bidderMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo

  let mp = policy terms
      voucherCS = VoucherCS $ scriptCurrencySymbol mp
      bidDepositDatum = BidDepositDatum (toPlutusKeyHash $ verificationKeyHash bidderVk) voucherCS
      bidDepositTxOut =
        TxOut
          (ShelleyAddressInEra depositAddress)
          depositValue
          (mkInlineDatum bidDepositDatum)
          ReferenceScriptNone
      depositValue = lovelaceToValue (Lovelace (naturalToInt depositAmount))

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
        , validityBound = (Nothing, Just $ biddingStart terms)
        }

losingBidderClaimDeposit :: AuctionTerms -> Runner ()
losingBidderClaimDeposit terms = do
  logMsg "Claiming bidder deposit"

  (bidderAddress, bidderVk, bidderSk) <- addressAndKeys

  bidderMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo
  standingBidUtxo <- scriptUtxos StandingBid terms
  allDeposits <- scriptUtxos Deposit terms

  deposit <- findDepositMatchingPubKeyHash terms (toPlutusKeyHash $ verificationKeyHash bidderVk) allDeposits

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
        , validityBound = (Just $ biddingEnd terms, Nothing)
        }
  where
    depositScript = scriptPlutusScript Deposit terms
    depositWitness = mkInlinedDatumScriptWitness depositScript LosingBidder

sellerClaimDepositFor :: AuctionTerms -> PubKeyHash -> Runner ()
sellerClaimDepositFor terms bidderPkh = do
  logMsg "Seller claiming bidder deposit"

  (sellerAddress, _, sellerSk) <- addressAndKeys

  sellerMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo
  standingBidUtxo <- scriptUtxos StandingBid terms
  auctionEscrowUtxo <- scriptUtxos Escrow terms

  allDeposits <- scriptUtxos Deposit terms

  deposit <- findDepositMatchingPubKeyHash terms bidderPkh allDeposits

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
        , validityBound = (Just $ voucherExpiry terms, Nothing)
        }
  where
    depositScript = scriptPlutusScript Deposit terms
    depositWitness = mkInlinedDatumScriptWitness depositScript SellerClaimsDeposit

cleanupDeposit :: AuctionTerms -> Runner ()
cleanupDeposit terms = do
  logMsg "Cleanup bidder deposit"

  (bidderAddress, bidderVk, bidderSk) <- addressAndKeys

  bidderMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo

  allDeposits <- scriptUtxos Deposit terms

  deposit <- findDepositMatchingPubKeyHash terms (toPlutusKeyHash $ verificationKeyHash bidderVk) allDeposits
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
        , validityBound = (Just $ cleanup terms, Nothing)
        }
  where
    depositScript = scriptPlutusScript Deposit terms
    depositWitness = mkInlinedDatumScriptWitness depositScript CleanupDeposit
