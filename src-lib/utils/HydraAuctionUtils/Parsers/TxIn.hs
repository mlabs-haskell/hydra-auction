module HydraAuctionUtils.Parsers.TxIn (
  parseTxIn,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as BSC
import Options.Applicative (ReadM, eitherReader, some)
import Text.Parsec ((<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Error qualified as Parsec
import Text.Parsec.Language qualified as Parsec
import Text.Parsec.String qualified as Parsec
import Text.Parsec.Token qualified as Parsec

-- Cardano node imports
import Cardano.Api (
  AsType (AsTxId),
  TxId (..),
  TxIn (..),
  TxIx (..),
  deserialiseFromRawBytesHex,
  displayError,
 )

parseTxIn :: ReadM TxIn
parseTxIn = readerFromParsecParser parseTxIn'

parseTxIn' :: Parsec.Parser TxIn
parseTxIn' = TxIn <$> parseTxId <*> (Parsec.char '#' *> parseTxIx)

parseTxId :: Parsec.Parser TxId
parseTxId = do
  strTxId <- some Parsec.hexDigit <?> "transaction id (hexadecimal)"
  case deserialiseFromRawBytesHex AsTxId (BSC.pack strTxId) of
    Right addr -> return addr
    Left e -> fail $ "Incorrect transaction id format: " <> displayError e

parseTxIx :: Parsec.Parser TxIx
parseTxIx = TxIx . fromIntegral <$> decimal

decimal :: Parsec.Parser Integer
Parsec.TokenParser {Parsec.decimal = decimal} = Parsec.haskell

readerFromParsecParser :: Parsec.Parser a -> ReadM a
readerFromParsecParser p =
  eitherReader (first formatError . Parsec.parse (p <* Parsec.eof) "")
  where
    -- FIXME: the default parsec error formatting is quite good, but we could
    -- customise it somewhat:
    formatError err =
      Parsec.showErrorMessages
        "or"
        "unknown parse error"
        "expecting"
        "unexpected"
        "end of input"
        (Parsec.errorMessages err)
