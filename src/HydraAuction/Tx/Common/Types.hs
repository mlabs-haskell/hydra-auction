{-# LANGUAGE StrictData #-}

module HydraAuction.Tx.Common.Types (
  AutoCreateParams (..),
) where

-- Preludes
import Prelude

-- Plutus
import Plutus.V1.Ledger.Api (POSIXTime)

-- Hydra
import Hydra.Cardano.Api (
  Address,
  BuildTx,
  BuildTxWith,
  CtxTx,
  Key (SigningKey),
  PaymentKey,
  ShelleyAddr,
  TxIn,
  TxMintValue,
  TxOut,
  UTxO,
  WitCtxTxIn,
  Witness,
 )

data AutoCreateParams = AutoCreateParams
  { authoredUtxos :: [(SigningKey PaymentKey, UTxO)]
  , -- | Utxo which TxIns will be used as reference inputs
    referenceUtxo :: UTxO
  , -- | Nothing means collateral will be chosen automatically from given UTxOs
    collateral :: Maybe TxIn
  , witnessedUtxos ::
      [(BuildTxWith BuildTx (Witness WitCtxTxIn), UTxO)]
  , outs :: [TxOut CtxTx]
  , toMint :: TxMintValue BuildTx
  , changeAddress :: Address ShelleyAddr
  , validityBound :: (Maybe POSIXTime, Maybe POSIXTime)
  }
