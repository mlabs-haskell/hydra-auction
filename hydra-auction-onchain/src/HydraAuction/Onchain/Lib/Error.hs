module HydraAuction.Onchain.Lib.Error (
  ToErrorCode (..),
  err,
  errMaybe,
  eCode,
) where

import PlutusTx.Prelude qualified as Plutus
import Prelude qualified as Haskell

import Data.Text qualified as Text
import Language.Haskell.TH (Exp (..), Lit (StringL), Q)

import HydraAuction.Error (ToErrorCode (..))

{-# INLINEABLE err #-}
err :: Plutus.Bool -> Plutus.BuiltinString -> Plutus.Bool
err x e = Plutus.traceIfFalse e x

{-# INLINEABLE errMaybe #-}
errMaybe :: Plutus.Maybe a -> Plutus.BuiltinString -> a
errMaybe mx e = Plutus.fromMaybe (Plutus.traceError e) mx

-- | Get the string literal from given error 'e'. Use this with template haskell
-- splices, e.g. $(eCode MyError)
eCode :: ToErrorCode e => e -> Q Exp
eCode e =
  Haskell.pure (LitE (StringL (Text.unpack (toErrorCode e))))
