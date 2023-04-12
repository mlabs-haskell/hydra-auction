module HydraAuction.Tx.Deposit (
  mkDeposit,
  losingBidderClaimDeposit,
  sellerClaimDepositFor,
  parseBidDepositDatum,
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
  TxOut,
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
import HydraAuction.Addresses (VoucherCS (..))
import HydraAuction.OnChain (AuctionScript (..), policy)
import HydraAuction.Runner (Runner)
import HydraAuction.Tx.Common (
  actorTipUtxo,
  addressAndKeys,
  minLovelace,
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

mkDeposit :: AuctionTerms -> Runner ()
mkDeposit terms = do
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
      -- FIXME: take as input?
      depositValue = lovelaceToValue minLovelace

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

  let mp = policy terms
      voucherCS = VoucherCS $ scriptCurrencySymbol mp
      expectedDatum = BidDepositDatum (toPlutusKeyHash $ verificationKeyHash bidderVk) voucherCS
      depositScript = scriptPlutusScript Deposit terms
      depositWitness = mkInlinedDatumScriptWitness depositScript LosingBidder

  case UTxO.find ((== expectedDatum) . parseBidDepositDatum) allDeposits of
    Nothing -> fail "Unable to find matching deposit"
    Just deposit -> do
      void $
        autoSubmitAndAwaitTx $
          AutoCreateParams
            { signedUtxos = [(bidderSk, bidderMoneyUtxo)]
            , additionalSigners = []
            , referenceUtxo = standingBidUtxo
            , witnessedUtxos = [(depositWitness, UTxO.singleton deposit)]
            , collateral = Nothing
            , outs = []
            , toMint = TxMintValueNone
            , changeAddress = bidderAddress
            , validityBound = (Just $ biddingEnd terms, Nothing)
            }

parseBidDepositDatum :: TxOut ctx -> BidDepositDatum
parseBidDepositDatum out = case txOutDatum out of
  TxOutDatumInline scriptData ->
    case fromData $ toPlutusData scriptData of
      Just bidDepositDatum -> bidDepositDatum
      Nothing ->
        error "Impossible happened: Cannot decode bid deposit datum"
  _ -> error "Impossible happened: No inline data for bid deposit"

sellerClaimDepositFor :: AuctionTerms -> PubKeyHash -> Runner ()
sellerClaimDepositFor terms bidderPkh = do
  logMsg "Seller claiming bidder deposit"

  (sellerAddress, _, sellerSk) <- addressAndKeys

  sellerMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo
  standingBidUtxo <- scriptUtxos StandingBid terms
  auctionEscrowUtxo <- scriptUtxos Escrow terms

  allDeposits <- scriptUtxos Deposit terms

  let mp = policy terms
      voucherCS = VoucherCS $ scriptCurrencySymbol mp
      expectedDatum = BidDepositDatum bidderPkh voucherCS
      depositScript = scriptPlutusScript Deposit terms
      depositWitness = mkInlinedDatumScriptWitness depositScript SellerClaimsDeposit

  case UTxO.find ((== expectedDatum) . parseBidDepositDatum) allDeposits of
    Nothing -> fail "Unable to find matching deposit"
    Just deposit -> do
      void $
        autoSubmitAndAwaitTx $
          AutoCreateParams
            { signedUtxos = [(sellerSk, sellerMoneyUtxo)]
            , additionalSigners = []
            , referenceUtxo = standingBidUtxo <> auctionEscrowUtxo
            , witnessedUtxos = [(depositWitness, UTxO.singleton deposit)]
            , collateral = Nothing
            , outs = []
            , toMint = TxMintValueNone
            , changeAddress = sellerAddress
            , validityBound = (Just $ voucherExpiry terms, Nothing)
            }
