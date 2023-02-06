module ParsingCliHelpers where

import Text.Parsec ((<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Error qualified as Parsec
import Text.Parsec.Language qualified as Parsec
import Text.Parsec.String qualified as Parsec
import Text.Parsec.Token qualified as Parsec

import Data.ByteString.Char8 qualified as BSC

parseActor "alice" = Alice
parseActor "bob" = Bob
parseActor "carol" = Carol
parseActor "faucet" = error "Not supported actor"
parseActor _ = error "Actor parsing error"

parseScript "escrow" = Escrow
parseScript "standing-bid" = StandingBid
parseScript "fee-escrow" = FeeEscrow
parseScript _ = error "Escrow parsing error"

parseTxIn :: Parsec.Parser TxIn
parseTxIn = TxIn <$> parseTxId <*> (Parsec.char '#' *> parseTxIx)

parseTxId :: Parsec.Parser TxId
parseTxId = do
  str <- some Parsec.hexDigit <?> "transaction id (hexadecimal)"
  case deserialiseFromRawBytesHex AsTxId (BSC.pack str) of
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
