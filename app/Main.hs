module Main (main) where

import Hydra.Prelude (toList)
import Prelude

import Cardano.Api (NetworkId (..))
import CardanoNode (RunningNode (RunningNode, networkId, nodeSocket), withCardanoNodeDevnet)
import Control.Monad (forM_)
import Data.Functor.Contravariant (contramap)
import Hydra.Cardano.Api (NetworkMagic (NetworkMagic))
import Hydra.Cluster.Faucet
import Hydra.Cluster.Fixture
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util
import HydraAuction.Tx.Common
import HydraAuction.Tx.Escrow
import HydraAuction.Tx.TestNFT
import Options.Applicative
import Test.Hydra.Prelude (withTempDir)

import Control.Concurrent (threadDelay)
import Hydra.Logging (Tracer, showLogsOnFailure, withTracerOutputTo)
import HydraAuction.OnChain
import HydraAuction.Tx.Common
import HydraNode (
  EndToEndLog (FromCardanoNode, FromFaucet),
 )
import System.FilePath ((</>))
import System.IO (IOMode (ReadWriteMode), withFile)

import Cardano.Api (AsType (AsTxId), TxId (..), TxIn (..), TxIx (..), deserialiseFromRawBytesHex, displayError)

import Data.Bifunctor (first)

import Text.Parsec ((<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Error qualified as Parsec
import Text.Parsec.Language qualified as Parsec
import Text.Parsec.String qualified as Parsec
import Text.Parsec.Token qualified as Parsec

import Data.ByteString.Char8 qualified as BSC

data CLIAction
  = RunCardanoNode
  | ShowEscrow
  | ShowScriptUtxos AuctionScript Actor TxIn
  | ShowUtxos Actor
  | Seed Actor
  | MintTestNFT Actor
  | AuctionAnounce Actor TxIn
  | StartBidding Actor TxIn

parseActor "alice" = Alice
parseActor "bob" = Bob
parseActor "carol" = Carol
parseActor "faucet" = error "Not supported actor"
parseActor _ = error "Actor parsing error"

parseScript "escrow" = Escrow
parseScript "standing-bid" = StandingBid
parseScript "fee-escrow" = FeeEscrow
parseScript _ = error "Escrow parsing error"

cliParser :: Parser CLIAction
cliParser =
  subparser
    ( command "run-cardano-node" (info (pure RunCardanoNode) (progDesc "TODO1"))
        <> command "show-script-utxos" (info (ShowScriptUtxos <$> script <*> actor <*> utxo) (progDesc "TODO1"))
        <> command "show-utxos" (info (ShowUtxos <$> actor) (progDesc "TODO1"))
        <> command "seed" (info (Seed <$> actor) (progDesc "TODO1"))
        <> command "mint-test-nft" (info (MintTestNFT <$> actor) (progDesc "TODO2"))
        <> command "announce-auction" (info (AuctionAnounce <$> actor <*> utxo) (progDesc "TODO2"))
        <> command "start-bidding" (info (StartBidding <$> actor <*> utxo) (progDesc "TODO2"))
    )
  where
    actor :: Parser Actor
    actor = parseActor <$> strOption (short 'a' <> metavar "ACTOR" <> help "Actor to use for tx and AuctionTerms construction")
    script :: Parser AuctionScript
    script = parseScript <$> strOption (short 's' <> metavar "SCRIPT" <> help "Script to check")
    utxo :: Parser TxIn
    utxo = option (readerFromParsecParser parseTxIn) (short 'u' <> metavar "UTXO" <> help "Utxo with test NFT for AuctionTerms")

opts :: ParserInfo CLIAction
opts =
  info
    cliParser
    ( fullDesc
        <> progDesc "TODO"
        <> header "TODO"
    )

prettyPrintUtxo utxo = do
  putStrLn "Utxos: \n"
  -- TODO print properly
  forM_ (toList utxo) $ \x ->
    putStrLn $ show x

main :: IO ()
main = do
  action <- execParser opts
  let gotNode = pure $ RunningNode {nodeSocket = "./node.socket", networkId = Testnet $ NetworkMagic 42}
  case action of
    RunCardanoNode -> do
      putStrLn "Running cardano-node"
      withTempDir "hydra-auction-1" $ \workDir -> do
        withFile (workDir </> "test.log") ReadWriteMode $ \hdl ->
          showLogsOnFailure $ \tracer -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) "." $ \node@RunningNode {nodeSocket, networkId} -> do
              -- TODO: proper working dir
              -- Somehow it hangs without infinite loop "/
              return ()
    Seed actor -> do
      showLogsOnFailure $ \tracer -> do
        node <- gotNode
        (key, _) <- keysFor actor
        seedFromFaucet_ node key 100_000_000 Normal (contramap FromFaucet tracer)
    ShowScriptUtxos script actor utxoRef -> do
      node <- gotNode
      terms <- constructTerms node actor utxoRef
      utxos <- scriptUtxos node script terms
      prettyPrintUtxo utxos
    ShowUtxos actor -> do
      node <- gotNode
      utxos <- actorTipUtxo node actor
      prettyPrintUtxo utxos
    MintTestNFT actor -> do
      node <- gotNode
      mintOneTestNFT node actor
    AuctionAnounce actor utxo -> do
      node <- gotNode
      terms <- constructTerms node actor utxo
      announceAuction node actor terms utxo
    StartBidding actor utxo -> do
      node <- gotNode
      terms <- constructTerms node actor utxo
      startBidding node actor terms utxo

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
    --TODO: the default parsec error formatting is quite good, but we could
    -- customise it somewhat:
    formatError err =
      Parsec.showErrorMessages
        "or"
        "unknown parse error"
        "expecting"
        "unexpected"
        "end of input"
        (Parsec.errorMessages err)
