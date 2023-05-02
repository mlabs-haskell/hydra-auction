{-# OPTIONS -Wno-orphans #-}
module HydraAuctionUtils.PrettyPrinting (prettyPrintUtxo) where

-- Prelude imports
import Hydra.Prelude (fromString)
import Prelude

-- Haskell imports
import Data.Map.Strict qualified as Map
import Prettyprinter (Doc, Pretty (pretty), punctuate, sep, vsep)
import Prettyprinter.Util (putDocW)

-- Cardano node imports
import Cardano.Api (TxOut (..))
import Cardano.Api.UTxO (UTxO, toMap)

-- Hydra imports
import Hydra.Cardano.Api (
  AssetId (..),
  Lovelace (..),
  Quantity (..),
  TxId (..),
  TxIn (..),
  TxIx (..),
  TxOutValue (..),
  filterValue,
  txOutValueToLovelace,
  txOutValueToValue,
  valueToList,
 )

-- Orphan instances for Caradano API

addressLikeShowStringDoc :: forall a. String -> Doc a
addressLikeShowStringDoc stringWithQuotes =
  let stringWithoutQuotes =
        drop 1 $
          take (length stringWithQuotes - 1) stringWithQuotes
   in fromString $ "< " <> stringWithoutQuotes <> " >"

instance Pretty TxId where
  pretty (TxId txId) = addressLikeShowStringDoc $ show txId

instance Pretty TxIn where
  pretty (TxIn txId (TxIx txIxWord)) =
    pretty txId <> " #" <> pretty txIxWord

instance Pretty AssetId where
  pretty (AssetId policy name) =
    addressLikeShowStringDoc (show policy) <> " " <> fromString (show name)
  pretty AdaAssetId = "ADA"

instance Pretty (TxOutValue _a) where
  pretty txOutValue =
    let nonAdaValues =
          valueToList $
            filterValue (/= AdaAssetId) $
              txOutValueToValue txOutValue
        nonAdaDocs =
          [ "  + " <> pretty q <> " " <> pretty ai
          | (ai, Quantity q) <- nonAdaValues
          ]
        (Lovelace adaValue) = txOutValueToLovelace txOutValue
        adaNumDoc = pretty $ (fromIntegral adaValue :: Double) / 1_000_000
        adaDoc = "ADA " <> adaNumDoc
     in vsep $ adaDoc : nonAdaDocs

-- Utility functions

prettyPrintUtxo :: UTxO -> IO ()
prettyPrintUtxo utxo = do
  putStrLn "Utxos: \n"

  let docs =
        [ pretty txIn <> ": " <> pretty txOutValue
        | (txIn, TxOut _ txOutValue _ _) <- Map.toList $ toMap utxo
        ]

  putDocW 80 (sep $ punctuate "\n" docs)

  putStrLn "\n"
