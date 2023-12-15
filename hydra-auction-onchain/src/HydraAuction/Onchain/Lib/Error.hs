module HydraAuction.Onchain.Lib.Error (
  ErrorCode (..),
  err,
  errMaybe,
  errMaybeFlip,
  eCode,
) where

import PlutusTx.Prelude qualified as Plutus

import HydraAuction.Error (ErrorCode (..), eCode)

err :: Plutus.Bool -> Plutus.BuiltinString -> Plutus.Bool
err x e = Plutus.traceIfFalse e x
--
{-# INLINEABLE err #-}

errMaybe :: Plutus.Maybe a -> Plutus.BuiltinString -> a
errMaybe mx e = Plutus.fromMaybe (Plutus.traceError e) mx
--
{-# INLINEABLE errMaybe #-}

errMaybeFlip :: Plutus.BuiltinString -> Plutus.Maybe a -> a
errMaybeFlip = Plutus.flip errMaybe
--
{-# INLINEABLE errMaybeFlip #-}
