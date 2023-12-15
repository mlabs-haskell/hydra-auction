module Plutus.Cardano.Api.Codec.Base (
  -- Offchain -> Onchain
  toPlutusBytestring,
  toPlutusTextUtf8,
  -- Onchain -> Offchain
  fromPlutusBytestring,
  fromPlutusTextUtf8,
) where

import Prelude

import Data.Function ((&))

import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding

import PlutusTx.Prelude qualified as Plutus

-- -------------------------------------------------------------------------
-- Text
-- -------------------------------------------------------------------------
toPlutusTextUtf8 :: Text.Text -> Plutus.BuiltinByteString
toPlutusTextUtf8 cardanoText =
  cardanoText
    & Text.Encoding.encodeUtf8
    & Plutus.toBuiltin

fromPlutusTextUtf8 :: Plutus.BuiltinByteString -> Maybe Text.Text
fromPlutusTextUtf8 plutusBBS =
  plutusBBS
    & Plutus.fromBuiltin
    & Text.Encoding.decodeUtf8'
    & either (const Nothing) Just

-- -------------------------------------------------------------------------
-- Bytestring
-- -------------------------------------------------------------------------
toPlutusBytestring :: BS.ByteString -> Plutus.BuiltinByteString
toPlutusBytestring = Plutus.toBuiltin

fromPlutusBytestring :: Plutus.BuiltinByteString -> BS.ByteString
fromPlutusBytestring = Plutus.fromBuiltin
