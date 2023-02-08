module Main (main) where

import Hydra.Prelude (toList)
import Prelude

import Cardano.Api (NetworkId (..), TxIn)
import CardanoNode (RunningNode (RunningNode, networkId, nodeSocket), withCardanoNodeDevnet)
import Control.Monad (forM_, void)
import Data.Aeson (decode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as B
import Data.Functor.Contravariant (contramap)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Hydra.Cardano.Api (NetworkMagic (NetworkMagic))
import Hydra.Cluster.Faucet
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util
import HydraAuction.Tx.Common
import HydraAuction.Tx.Escrow
import HydraAuction.Tx.StandingBid
import HydraAuction.Tx.TestNFT
import HydraAuction.Types (Natural)
import Options.Applicative hiding (action)
import Test.Hydra.Prelude (withTempDir)

import Hydra.Logging (showLogsOnFailure)
import HydraAuction.OnChain
import HydraNode (
  EndToEndLog (FromCardanoNode, FromFaucet),
 )
import System.FilePath ((</>))
import System.IO (IOMode (ReadWriteMode), withFile)

import Data.Time.Clock.POSIX qualified as POSIXTime
import Plutus.V2.Ledger.Api (POSIXTime (..))

import ParsingCliHelpers

data CLIAction
  = RunCardanoNode
  | ShowScriptUtxos !AuctionScript !Actor !TxIn
  | ShowUtxos !Actor
  | Seed !Actor
  | MintTestNFT !Actor
  | AuctionAnounce !Actor !TxIn
  | StartBidding !Actor !TxIn
  | NewBid !Actor !TxIn !Actor !Natural
  | BidderBuys !Actor !TxIn !Actor
  | SellerReclaims !Actor !TxIn !Actor

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
        <> command "new-bid" (info (NewBid <$> actor <*> utxo <*> bidder <*> bidAmount) (progDesc "FIXME: add help message"))
        <> command "bidder-buys" (info (BidderBuys <$> actor <*> utxo <*> bidder) (progDesc "FIXME: add help message"))
        <> command "seller-reclaims" (info (SellerReclaims <$> actor <*> utxo <*> bidder) (progDesc "FIXME: add help message"))
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
    bidder :: Parser Actor
    bidder =
      parseActor
        <$> strOption
          ( short 'r'
              <> metavar "BIDDER"
              <> help "Actor for which we will place a bid"
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
    bidAmount :: Parser Natural
    bidAmount =
      parseNatural
        <$> strOption
          ( short 'b'
              <> metavar "BID_AMOUNT"
              <> help "Bid amount"
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

  let config =
        AuctionTermsConfig
          { deltaBiddingStart = 0
          , deltaBiddingEnd = 90
          , deltaVoucherExpiry = 150
          , deltaCleanup = 200
          , configAuctionFee = 4_000_000
          , configStartingBid = 8_000_000
          , configMinIncrement = 8_000_000
          }

  case action of
    RunCardanoNode -> do
      putStrLn "Running cardano-node"
      withTempDir "hydra-auction-1" $ \workDir -> do
        -- TODO: proper working dir
        withFile (workDir </> "test.log") ReadWriteMode $ \_hdl ->
          showLogsOnFailure $ \tracer -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) "." $
              -- Somehow it hangs without infinite loop "/
              error "Not implemented: RunCardanoNode"
    Seed actor -> do
      showLogsOnFailure $ \tracer -> do
        (key, _) <- keysFor actor
        seedFromFaucet_ node key 100_000_000 Normal (contramap FromFaucet tracer)
    ShowScriptUtxos script actor utxoRef -> do
      currentTime <- round <$> POSIXTime.getPOSIXTime
      terms <- constructTerms node currentTime config actor utxoRef
      utxos <- scriptUtxos node script terms
      prettyPrintUtxo utxos
    ShowUtxos actor -> do
      utxos <- actorTipUtxo node actor
      prettyPrintUtxo utxos
    MintTestNFT actor -> do
      void $ mintOneTestNFT node actor
    AuctionAnounce actor utxo -> do
      currentTime <- round <$> POSIXTime.getPOSIXTime
      writeAuctionTime currentTime
      terms <- constructTerms node currentTime config actor utxo
      announceAuction node actor terms
    StartBidding actor utxo -> do
      currentTime <- readAuctionTime
      terms <- constructTerms node currentTime config actor utxo
      startBidding node actor terms
    NewBid actor utxo bidder bidAmount -> do
      currentTime <- readAuctionTime
      terms <- constructTerms node currentTime config actor utxo
      newBid node bidder terms bidAmount
    BidderBuys actor utxo bidder -> do
      currentTime <- readAuctionTime
      terms <- constructTerms node currentTime config actor utxo
      bidderBuys node bidder terms
    SellerReclaims actor utxo bidder -> do
      currentTime <- readAuctionTime
      terms <- constructTerms node currentTime config actor utxo
      sellerReclaims node bidder terms

auctionFile :: FilePath
auctionFile = "auction_time.txt"

readAuctionTime :: IO POSIXTime
readAuctionTime = do
  POSIXTime . read . show <$> BS.readFile auctionFile

writeAuctionTime :: POSIXTime -> IO ()
writeAuctionTime (POSIXTime t) =
  BS.writeFile auctionFile . fromString . show $ t
