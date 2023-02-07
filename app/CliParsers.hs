module CliParsers (
  getCliAction,
) where

import Prelude

import Hydra.Cluster.Fixture (Actor (..))
import Options.Applicative

import HydraAuction.OnChain

import Cardano.Api (TxIn)

import CliActions (CliAction (..))
import ParseTxIn (parseTxIn)

getCliAction :: IO CliAction
getCliAction =
  execParser $
    info
      cliAction
      ( fullDesc
          <> progDesc "FIXME: add help message"
          <> header "FIXME: add help message"
      )

cliAction :: Parser CliAction
cliAction =
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
    parseTxIn
    ( short 'u'
        <> metavar "UTXO"
        <> help "Utxo with test NFT for AuctionTerms"
    )

parseActor :: String -> Actor
parseActor "alice" = Alice
parseActor "bob" = Bob
parseActor "carol" = Carol
parseActor "faucet" = error "Unsupported actor"
parseActor _ = error "Actor parsing error"

parseScript :: String -> AuctionScript
parseScript "escrow" = Escrow
parseScript "standing-bid" = StandingBid
parseScript "fee-escrow" = FeeEscrow
parseScript _ = error "Escrow parsing error"
