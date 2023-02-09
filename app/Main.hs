{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Prelude

import Cardano.Api (NetworkId (..), TxIn)
import CardanoNode (
  RunningNode (
    RunningNode,
    networkId,
    nodeSocket
  ),
  withCardanoNodeDevnet,
 )
import Control.Monad (forM_, void)
import Data.Functor.Contravariant (contramap)
import Hydra.Cardano.Api (NetworkMagic (NetworkMagic))
import Hydra.Cluster.Fixture (Actor (..))
import HydraAuction.Tx.Common
import HydraAuction.Tx.Escrow
import HydraAuction.Tx.TestNFT
import Options.Applicative
import Test.Hydra.Prelude (withTempDir)

import Hydra.Logging (
  Verbosity (Quiet, Verbose),
 )
import Hydra.Prelude (liftIO, toList)
import HydraAuction.OnChain
import HydraAuction.Runner
import HydraNode (
  EndToEndLog (
    FromCardanoNode
  ),
 )

import ParsingCliHelpers

data Options = MkOptions
  { cmd :: Command
  , verbosity :: Verbosity
  }

data Command
  = RunCardanoNode
  | ShowScriptUtxos !AuctionScript !Actor !TxIn
  | ShowUtxos !Actor
  | Seed !Actor
  | MintTestNFT !Actor
  | AuctionAnounce !Actor !TxIn
  | StartBidding !Actor !TxIn
  | BidderBuys !Actor !TxIn
  | SellerReclaims !Actor !TxIn

verboseParser :: Parser Bool
verboseParser = switch (long "verbose" <> short 'v')

optionsParser :: Parser Options
optionsParser =
  MkOptions <$> commandParser <*> (toVerbosity <$> verboseParser)
  where
    toVerbosity = \case
      True -> Verbose "hydra-auction"
      _ -> Quiet

commandParser :: Parser Command
commandParser =
  subparser $
    mconcat
      [ command
          "run-cardano-node"
          $ info
            (pure RunCardanoNode)
            (progDesc "FIXME: add help message")
      , command
          "show-script-utxos"
          $ info
            (ShowScriptUtxos <$> script <*> actor <*> utxo)
            (progDesc "FIXME: add help message")
      , command
          "show-utxos"
          $ info
            (ShowUtxos <$> actor)
            (progDesc "FIXME: add help message")
      , command
          "seed"
          $ info
            (Seed <$> actor)
            (progDesc "FIXME: add help message")
      , command
          "mint-test-nft"
          $ info
            (MintTestNFT <$> actor)
            (progDesc "FIXME: add help message")
      , command
          "announce-auction"
          $ info
            (AuctionAnounce <$> actor <*> utxo)
            (progDesc "FIXME: add help message")
      , command
          "start-bidding"
          $ info
            (StartBidding <$> actor <*> utxo)
            (progDesc "FIXME: add help message")
      , command
          "bidder-buys"
          $ info
            (BidderBuys <$> actor <*> utxo)
            (progDesc "FIXME: add help message")
      ]
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

opts :: ParserInfo Options
opts =
  info
    optionsParser
    $ fullDesc
      <> progDesc "FIXME: add help message"
      <> header "FIXME: add help message"

prettyPrintUtxo :: (Foldable t, Show a) => t a -> IO ()
prettyPrintUtxo utxo = do
  putStrLn "Utxos: \n"
  -- FIXME print properly
  forM_ (toList utxo) $ \x ->
    putStrLn $ show x

main :: IO ()
main = do
  MkOptions {..} <- execParser opts
  tracer <- stdoutTracer verbosity

  let node =
        RunningNode
          { nodeSocket = "./node.socket"
          , networkId = Testnet $ NetworkMagic 42
          }

  case cmd of
    RunCardanoNode -> do
      putStrLn "Running cardano-node"
      withTempDir "hydra-auction-1" $ \workDir -> do
        withCardanoNodeDevnet
          (contramap FromCardanoNode tracer)
          workDir
          $ \_ ->
            error "Not implemented: RunCardanoNode"
    -- TODO: proper working dir
    -- Somehow it hangs without infinite loop "/
    Seed actor -> do
      executeRunner tracer node $
        initWallet actor 100_000_000
    ShowScriptUtxos script actor utxoRef -> do
      terms <- constructTerms actor utxoRef
      utxos <- scriptUtxos node script terms
      prettyPrintUtxo utxos
    ShowUtxos actor -> do
      utxos <- actorTipUtxo node actor
      prettyPrintUtxo utxos
    MintTestNFT actor -> do
      executeRunner tracer node $
        void $ mintOneTestNFT actor
    AuctionAnounce actor utxo ->
      executeRunner tracer node $ do
        terms <- liftIO $ constructTerms actor utxo
        announceAuction actor terms
    StartBidding actor utxo ->
      executeRunner tracer node $ do
        terms <- liftIO $ constructTerms actor utxo
        startBidding actor terms
    BidderBuys actor utxo ->
      executeRunner tracer node $ do
        terms <- liftIO $ constructTerms actor utxo
        bidderBuys actor terms
    SellerReclaims actor utxo ->
      executeRunner tracer node $ do
        terms <- liftIO $ constructTerms actor utxo
        sellerReclaims actor terms
