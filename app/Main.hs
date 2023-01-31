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
import HydraNode (
  EndToEndLog (FromCardanoNode, FromFaucet),
 )
import System.FilePath ((</>))
import System.IO (IOMode (ReadWriteMode), withFile)

data CLIAction = RunCardanoNode | ShowUtxos Actor | Seed Actor | MintTestNFT Actor | AuctionAnounce Actor deriving stock (Show, Eq)

parseActor "alice" = Alice
parseActor "bob" = Bob
parseActor "carol" = Carol
parseActor "faucet" = error "Not supported actor"
parseActor _ = error "Actor parsing error"

cliParser :: Parser CLIAction
cliParser =
  subparser
    ( command "run-cardano-node" (info (pure RunCardanoNode) (progDesc "TODO1"))
        <> command "show-utxos" (info (ShowUtxos <$> actor) (progDesc "TODO1"))
        <> command "seed" (info (Seed <$> actor) (progDesc "TODO1"))
        <> command "mint-test-nft" (info (MintTestNFT <$> actor) (progDesc "TODO2"))
        <> command "announce-auction" (info (AuctionAnounce <$> actor) (progDesc "TODO2"))
    )
  where
    actor :: Parser Actor
    actor = parseActor <$> strOption (short 'a' <> metavar "ACTOR" <> help "Actor to use for tx")
    utxo = strOption (short 'u' <> metavar "UTXO" <> help "Utxo with test NFT for auction")

opts :: ParserInfo CLIAction
opts =
  info
    cliParser
    ( fullDesc
        <> progDesc "TODO"
        <> header "TODO"
    )

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
              return ()
    -- Somehow it hangs without infinite loop "/
    -- let infiniteLoop = do
    --         threadDelay 1_000_000
    --         infiniteLoop
    -- in infiniteLoop
    Seed actor -> do
      showLogsOnFailure $ \tracer -> do
        node <- gotNode
        (key, _) <- keysFor actor
        seedFromFaucet_ node key 100_000_000 Normal (contramap FromFaucet tracer)
    ShowUtxos actor -> do
      node <- gotNode
      -- TODO print properly
      putStrLn "Utxos: \n"
      utxo <- actorTipUtxo node actor
      forM_ (toList utxo) $ \x ->
        putStrLn $ show x
    MintTestNFT actor -> do
      node <- gotNode
      mintOneTestNFT node actor
    AuctionAnounce actor -> do
      node <- gotNode
      terms <- constructTerms node actor
      announceAuction node actor terms
