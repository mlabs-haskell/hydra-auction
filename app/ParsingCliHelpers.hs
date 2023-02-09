module ParsingCliHelpers (
  parseActor,
  parseScript,
  parseTxId,
  parseTxIn,
  parseTxIx,
  readerFromParsecParser,
) where

import Text.Parsec ((<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Error qualified as Parsec
import Text.Parsec.Language qualified as Parsec
import Text.Parsec.String qualified as Parsec
import Text.Parsec.Token qualified as Parsec

import Data.ByteString.Char8 qualified as BSC

import Prelude

import Hydra.Cluster.Fixture (Actor (..))
import Options.Applicative

import HydraAuction.OnChain

import Cardano.Api (
  AsType (AsTxId),
  TxId (..),
  TxIn (..),
  TxIx (..),
  deserialiseFromRawBytesHex,
  displayError,
 )

import Data.Bifunctor (first)

parseActor :: String -> Actor
parseActor "alice" = Alice
parseActor "bob" = Bob
parseActor "carol" = Carol
parseActor "faucet" = error "Not supported actor"
parseActor _ = error "Actor parsing error"

parseScript :: String -> AuctionScript
parseScript "escrow" = Escrow
parseScript "standing-bid" = StandingBid
parseScript "fee-escrow" = FeeEscrow
parseScript _ = error "Escrow parsing error"

parseTxIn :: Parsec.Parser TxIn
parseTxIn = TxIn <$> parseTxId <*> (Parsec.char '#' *> parseTxIx)

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
