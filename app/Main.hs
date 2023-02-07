module Main (main) where

import Hydra.Prelude (toList)
import Prelude

import Cardano.Api (NetworkId (..), TxIn)
import CardanoNode (RunningNode (RunningNode, networkId, nodeSocket), withCardanoNodeDevnet)
import Control.Monad (forM_, void)
import Data.Functor.Contravariant (contramap)
import Hydra.Cardano.Api (NetworkMagic (NetworkMagic))
import Hydra.Cluster.Faucet
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util
import HydraAuction.Tx.Common
import HydraAuction.Tx.Escrow
import HydraAuction.Tx.TestNFT
import Options.Applicative
import Test.Hydra.Prelude (withTempDir)

import Hydra.Logging (showLogsOnFailure)
import HydraAuction.OnChain
import HydraNode (
  EndToEndLog (FromCardanoNode, FromFaucet),
 )
import System.FilePath ((</>))
import System.IO (IOMode (ReadWriteMode), withFile)

import ParsingCliHelpers

data CLIAction
  = RunCardanoNode
  | ShowScriptUtxos !AuctionScript !Actor !TxIn
  | ShowUtxos !Actor
  | Seed !Actor
  | MintTestNFT !Actor
  | AuctionAnounce !Actor !TxIn
  | StartBidding !Actor !TxIn
  | BidderBuys !Actor !TxIn
  | SellerReclaims !Actor !TxIn

cliParser :: Parser CLIAction
cliParser =
  subparser
    ( command "run-cardano-node" (info (pure RunCardanoNode) (progDesc "FIXME: add help message"))
        <> command "show-script-utxos" (info (ShowScriptUtxos <$> script <*> actor <*> utxo) (progDesc "FIXME: add help message"))
        <> command "show-utxos" (info (ShowUtxos <$> actor) (progDesc "FIXME: add help message"))
        <> command "seed" (info (Seed <$> actor) (progDesc "FIXME: add help message"))
        <> command "mint-test-nft" (info (MintTestNFT <$> actor) (progDesc "FIXME: add help message"))
        <> command "announce-auction" (info (AuctionAnounce <$> actor <*> utxo) (progDesc "FIXME: add help message"))
        <> command "start-bidding" (info (StartBidding <$> actor <*> utxo) (progDesc "FIXME: add help message"))
        <> command "bidder-buys" (info (BidderBuys <$> actor <*> utxo) (progDesc "FIXME: add help message"))
    )
  where
    actor :: Parser Actor
    actor =
      parseActor
        <$> strOption
          ( short 'a'
              <> metavar "ACTOR"
              <> help "Actor to use for tx and AuctionTerms construction"
          )
    script :: Parser AuctionScript
    script =
      parseScript
        <$> strOption
          ( short 's'
              <> metavar "SCRIPT"
              <> help "Script to check"
          )
    utxo :: Parser TxIn
    utxo =
      option
        (readerFromParsecParser parseTxIn)
        ( short 'u'
            <> metavar "UTXO"
            <> help "Utxo with test NFT for AuctionTerms"
        )

opts :: ParserInfo CLIAction
opts =
  info
    cliParser
    ( fullDesc
        <> progDesc "FIXME: add help message"
        <> header "FIXME: add help message"
    )

prettyPrintUtxo :: (Foldable t, Show a) => t a -> IO ()
prettyPrintUtxo utxo = do
  putStrLn "Utxos: \n"
  -- FIXME print properly
  forM_ (toList utxo) $ \x ->
    putStrLn $ show x

main :: IO ()
main = do
  action <- execParser opts
  let node = RunningNode {nodeSocket = "./node.socket", networkId = Testnet $ NetworkMagic 42}
  case action of
    RunCardanoNode -> do
      putStrLn "Running cardano-node"
      withTempDir "hydra-auction-1" $ \workDir -> do
        withFile (workDir </> "test.log") ReadWriteMode $ \_hdl ->
          showLogsOnFailure $ \tracer -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) "." $
              error "Not implemented: RunCardanoNode"
    -- TODO: proper working dir
    -- Somehow it hangs without infinite loop "/
    Seed actor -> do
      showLogsOnFailure $ \tracer -> do
        (key, _) <- keysFor actor
        seedFromFaucet_ node key 100_000_000 Normal (contramap FromFaucet tracer)
    ShowScriptUtxos script actor utxoRef -> do
      terms <- constructTerms node actor utxoRef
      utxos <- scriptUtxos node script terms
      prettyPrintUtxo utxos
    ShowUtxos actor -> do
      utxos <- actorTipUtxo node actor
      prettyPrintUtxo utxos
    MintTestNFT actor -> do
      void $ mintOneTestNFT node actor
    AuctionAnounce actor utxo -> do
      terms <- constructTerms node actor utxo
      announceAuction node actor terms
    StartBidding actor utxo -> do
      terms <- constructTerms node actor utxo
      startBidding node actor terms
    BidderBuys actor utxo -> do
      terms <- constructTerms node actor utxo
      bidderBuys node actor terms
    SellerReclaims actor utxo -> do
      terms <- constructTerms node actor utxo
      sellerReclaims node actor terms
