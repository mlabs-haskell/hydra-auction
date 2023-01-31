module Main (main) where

import Prelude

import Cardano.Api (NetworkId (..))
import CardanoNode (RunningNode (RunningNode, networkId, nodeSocket), withCardanoNodeDevnet)
import Data.Functor.Contravariant (contramap)
import Hydra.Cluster.Fixture (Actor (..))
import HydraAuction.Tx.TestNFT
import Options.Applicative
import Test.Hydra.Prelude (withTempDir)

import Control.Concurrent (threadDelay)

-- TODO

import Hydra.Logging (Tracer, showLogsOnFailure, withTracerOutputTo)
import HydraNode (
  EndToEndLog (FromCardanoNode, FromFaucet),
 )
import System.FilePath ((</>))
import System.IO (IOMode (ReadWriteMode), withFile)

data CLIAction = RunCardanoNode | MintTestNFT | AuctionAnounce String deriving stock (Show, Eq)

cliParser :: Parser CLIAction
cliParser =
  subparser
    ( command "run-cardano-node" (info (pure RunCardanoNode) (progDesc "TODO1"))
        <> command "mint-test-nft" (info (pure MintTestNFT) (progDesc "TODO2"))
        <> command "announce-auction" (info auctionAnounce (progDesc "TODO2"))
    )
  where
    auctionAnounce = AuctionAnounce <$> utxo
    utxo = strOption (short 'u' <> metavar "Utxo" <> help "Utxo with test NFT for auction")

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
  case action of
    RunCardanoNode -> do
      withTempDir "hydra-cluster-2" $ \workDir ->
        withFile (workDir </> "test.log") ReadWriteMode $ \hdl ->
          showLogsOnFailure $ \tracer -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) "." $ \node@RunningNode {nodeSocket, networkId} -> do
              putStrLn "TEST"
              -- TOOD: put node to disk
              pure ()
    -- let infiniteLoop = do
    --         threadDelay 1_000_000
    --         infiniteLoop
    -- in infiniteLoop
    MintTestNFT -> do
      pure ()

-- read node from disk
-- mintOneTestNFT node Alice
