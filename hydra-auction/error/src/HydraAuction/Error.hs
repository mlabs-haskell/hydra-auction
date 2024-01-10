module HydraAuction.Error (
  ErrorCodePrefix (..),
  ErrorCode (..),
  eCode,
) where

import Prelude

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Universe (Universe (..))
import Safe (atMay)

import Language.Haskell.TH (Exp (..), Lit (StringL), Q)

-- | Prefix an error code with a short text tag.
-- Typically, this should be defined like this:
--   errorCodePrefix = const "ABCD"
--
-- Make sure that error code prefixes are unique per error type.
class ErrorCodePrefix a where
  errorCodePrefix :: proxy a -> Text

-- | Types which are used to describe errors as short error codes in scripts.
-- Laws:
--   1. fromErrorCode . toErrorCode = Just
--   2. (toErrorCode <$> fromErrorCode x) == (const x <$> fromErrorCode x)
class (ErrorCodePrefix a, Eq a, Universe a) => ErrorCode a where
  -- | Get the short error code used in a script for given error type.
  toErrorCode :: a -> Text

  -- | Get the error type from an error code,
  -- assuming that the error code produced from that error type.
  fromErrorCode :: Text -> Maybe a

-- | Sequentially ordered types have sequentially ordered error codes.
-- Assuming that Universe implementation is correct,
-- this instance should satisfy the ErrorCode laws.
instance (Universe a, Eq a, ErrorCodePrefix a) => ErrorCode a where
  toErrorCode x = prefix <> numericCode
    where
      -- fromJust should not result in an error here if Universe is correct.
      numericCode = Text.pack $ show $ fromJust $ elemIndex x universe
      prefix = errorCodePrefix (Proxy :: Proxy a)

  fromErrorCode x = atMay universe =<< numericCode
    where
      numericCode = read . Text.unpack <$> Text.stripPrefix prefix x
      prefix = errorCodePrefix (Proxy :: Proxy a)

-- | Get the string literal from given error 'e'.
-- Use this with template haskell splices, e.g. $(eCode MyError)
eCode :: (ErrorCode e) => e -> Q Exp
eCode e =
  pure (LitE (StringL (Text.unpack (toErrorCode e))))
