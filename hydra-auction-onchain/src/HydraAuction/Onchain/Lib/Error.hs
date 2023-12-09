module HydraAuction.Onchain.Lib.Error (
  ErrorCode (..),
  err,
  errMaybe,
  eCode,
) where

import PlutusTx.Prelude qualified as Plutus
import Prelude qualified as Haskell

import Data.Text qualified as Text
import Language.Haskell.TH (Exp (..), Lit (StringL), Q)

import HydraAuction.Error (ErrorCode (..))

err :: Plutus.Bool -> Plutus.BuiltinString -> Plutus.Bool
err x e = Plutus.traceIfFalse e x
--
{-# INLINEABLE err #-}

errMaybe :: Plutus.Maybe a -> Plutus.BuiltinString -> a
errMaybe mx e = Plutus.fromMaybe (Plutus.traceError e) mx
--
{-# INLINEABLE errMaybe #-}

-- | Get the string literal from given error 'e'. Use this with template haskell
-- splices, e.g. $(eCode MyError)
eCode :: ErrorCode e => e -> Q Exp
eCode e =
  Haskell.pure (LitE (StringL (Text.unpack (toErrorCode e))))
