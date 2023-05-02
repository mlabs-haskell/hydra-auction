module HydraAuction.Tx.FeeEscrow (
  distributeFee,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad (void)

-- Plutus imports
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.V2.Ledger.Api (PubKeyHash)

-- Cardano node imports
import Cardano.Api.UTxO qualified as UTxO

-- Hydra imports
import Hydra.Cardano.Api (
  Lovelace (..),
  Value,
  lovelaceToValue,
  txOutValue,
  valueToLovelace,
  pattern ReferenceScriptNone,
  pattern TxMintValueNone,
  pattern TxOut,
  pattern TxOutDatumNone,
 )

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Tx.Common (
  scriptPlutusScript,
  scriptUtxos,
 )
import HydraAuction.Types (
  AuctionTerms (..),
  FeeEscrowRedeemer (..),
 )
import HydraAuctionUtils.L1.Runner (L1Runner)
import HydraAuctionUtils.Monads (
  fromPlutusAddressInMonad,
  logMsg,
 )
import HydraAuctionUtils.Monads.Actors (
  actorTipUtxo,
  addressAndKeys,
 )
import HydraAuctionUtils.Tx.AutoCreateTx (
  AutoCreateParams (..),
  autoSubmitAndAwaitTx,
 )
import HydraAuctionUtils.Tx.Build (
  mkInlinedDatumScriptWitness,
 )
import HydraAuctionUtils.Tx.Utxo (
  filterAdaOnlyUtxo,
 )

distributeFee :: AuctionTerms -> L1Runner ()
distributeFee terms = do
  logMsg "Distributing fees to delegates"

  (actorAddress, _, actorSk) <- addressAndKeys

  actorMoneyUtxo <- filterAdaOnlyUtxo <$> actorTipUtxo

  feeEscrowUtxo <- scriptUtxos FeeEscrow terms

  feeEscrowValue <- case UTxO.pairs feeEscrowUtxo of
    [(_, txOut)] -> pure $ txOutValue txOut
    _ -> fail "wrong number of utxos in fee escrow script"

  lovelaceAmt <- case valueToLovelace feeEscrowValue of
    Just (Lovelace l) -> pure l
    Nothing -> fail "fee escrow asset does not contain ada-only"

  delegateOuts <- mapM mkFeeOut (zipDistributingValue lovelaceAmt (delegates terms))

  logMsg $ show delegateOuts
  void $
    autoSubmitAndAwaitTx $
      AutoCreateParams
        { signedUtxos = [(actorSk, actorMoneyUtxo)]
        , additionalSigners = []
        , referenceUtxo = mempty
        , witnessedUtxos = [(feeWitness, feeEscrowUtxo)]
        , collateral = Nothing
        , outs = delegateOuts
        , toMint = TxMintValueNone
        , changeAddress = actorAddress
        , validityBound = (Nothing, Nothing)
        }
  where
    feeScript = scriptPlutusScript FeeEscrow terms
    feeWitness = mkInlinedDatumScriptWitness feeScript DistributeFees

    mkFeeOut (delegatePKH, valuePerDelegate) = do
      delegateAddress <- fromPlutusAddressInMonad $ pubKeyHashAddress delegatePKH
      pure $
        TxOut
          delegateAddress
          valuePerDelegate
          TxOutDatumNone
          ReferenceScriptNone

zipDistributingValue :: Integer -> [PubKeyHash] -> [(PubKeyHash, Value)]
zipDistributingValue lovelaceAmt delegatePKHs = zip delegatePKHs (lovelaceToValue . Lovelace <$> distributedLovelace)
  where
    delegateNumber = length delegatePKHs
    (quotient, remainder) = divMod lovelaceAmt (toInteger delegateNumber)

    spread _ [] = []
    spread 0 xs = xs
    spread k (x : xs) = (x + 1) : spread (k - 1) xs

    distributedLovelace = spread remainder (replicate delegateNumber quotient)
