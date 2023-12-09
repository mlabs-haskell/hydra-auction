module HydraAuction.Error (
  ErrorCodePrefix (..),
  ErrorCode (..),
) where

import Prelude

import Data.List (elemIndex)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Universe (Universe (..))
import Safe (atMay)

-- | Prefix an error code with a short text tag.
-- Typically, this should be defined like this:
--   errorCodePrefix = const "ABCD"
--
-- Make sure that error code prefixes are unique per error type.
class ErrorCodePrefix a where
  errorCodePrefix :: proxy a -> Text

-- | Types which are used to describe errors as short error codes in scripts.
class (ErrorCodePrefix a, Eq a, Universe a) => ErrorCode a where
  -- | Get the short error code used in a script for given error type.
  toErrorCode :: a -> Text

  -- | Get the error type from an error code,
  -- assuming that the error code produced from that error type.
  fromErrorCode :: Text -> Maybe a

-- | Sequentially ordered types have sequentially ordered error codes.
instance (Universe a, Eq a, ErrorCodePrefix a) => ErrorCode a where
  toErrorCode x = prefix <> numericCode
    where
      prefix = errorCodePrefix (Proxy :: Proxy a)
      numericCode = Text.pack $ show $ elemIndex x universe

  fromErrorCode x = atMay universe =<< numericCode
    where
      prefix = errorCodePrefix (Proxy :: Proxy a)
      numericCode = read . Text.unpack <$> Text.stripPrefix prefix x
