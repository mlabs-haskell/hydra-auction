module HydraAuctionOnchain.Lib.Error (
  ToErrorCode (..),
  err,
  eCode,
) where

import PlutusTx.Prelude qualified as Plutus
import Prelude qualified as Haskell

import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH (Exp (..), Lit (StringL), Q)

{-# INLINEABLE err #-}
err :: Plutus.Bool -> Plutus.BuiltinString -> Plutus.Bool
err x e = Plutus.traceIfFalse e x

-- | Types which are used to describe errors as short error codes in scripts.
class ToErrorCode a where
  -- | Get the short error code used in a script for given type.
  toErrorCode :: a -> Text

-- | Get the string literal from given error 'e'. Use this with template haskell
-- splices, e.g. $(eCode MyError)
eCode :: ToErrorCode e => e -> Q Exp
eCode e =
  Haskell.pure (LitE (StringL (Text.unpack (toErrorCode e))))
