module CliActions (
  CliAction (..),
  handleCliAction,
  seedAmount,
) where

import Hydra.Prelude (toList)
import Prelude

import Cardano.Api (NetworkId (..), TxIn)
import CardanoNode (RunningNode (RunningNode, networkId, nodeSocket), withCardanoNodeDevnet)
import CliConfig (
  AuctionName,
  CLIEnhancedAuctionTerms (..),
  configToAuctionTerms,
  constructTermsDynamic,
  readAuctionTerms,
  readAuctionTermsConfig,
  readCLIEnhancedAuctionTerms,
  writeAuctionTermsDynamic,
 )
import Control.Monad (forM_, void)
import Data.Functor.Contravariant (contramap)
import Hydra.Cardano.Api (Lovelace, NetworkMagic (NetworkMagic))
import Hydra.Cluster.Faucet
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (keysFor)
import Hydra.Logging (showLogsOnFailure)
import HydraAuction.OnChain
import HydraAuction.Tx.Common
import HydraAuction.Tx.Escrow
import HydraAuction.Tx.TestNFT
import HydraNode (
  EndToEndLog (FromCardanoNode, FromFaucet),
 )
import System.FilePath ((</>))
import System.IO (IOMode (ReadWriteMode), withFile)
import Test.Hydra.Prelude (withTempDir)

seedAmount :: Lovelace
seedAmount = 100_000_000

data CliAction
  = RunCardanoNode
  | ShowScriptUtxos !AuctionName !AuctionScript
  | ShowUtxos !Actor
  | Seed !Actor
  | MintTestNFT !Actor
  | AuctionAnounce !AuctionName !Actor !TxIn
  | StartBidding !AuctionName
  | BidderBuys !AuctionName !Actor
  | SellerReclaims !AuctionName

handleCliAction :: CliAction -> IO ()
handleCliAction userAction =
  case userAction of
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
        node <- getNode
        (key, _) <- keysFor actor
        seedFromFaucet_ node key seedAmount Normal (contramap FromFaucet tracer)
    ShowScriptUtxos auctionName script -> do
      node <- getNode
      -- FIXME: proper error printing
      Just terms <- readAuctionTerms auctionName
      utxos <- scriptUtxos node script terms
      prettyPrintUtxo utxos
    ShowUtxos actor -> do
      node <- getNode
      utxos <- actorTipUtxo node actor
      prettyPrintUtxo utxos
    MintTestNFT actor -> do
      node <- getNode
      void $ mintOneTestNFT node actor
    AuctionAnounce auctionName sellerActor utxo -> do
      node <- getNode
      dynamic <- constructTermsDynamic sellerActor utxo
      writeAuctionTermsDynamic auctionName dynamic
      -- FIXME: proper error printing
      Just config <- readAuctionTermsConfig auctionName
      terms <- configToAuctionTerms config dynamic
      announceAuction node sellerActor terms
    StartBidding auctionName -> do
      node <- getNode
      -- FIXME: proper error printing
      Just (CLIEnhancedAuctionTerms {terms, sellerActor}) <- readCLIEnhancedAuctionTerms auctionName
      startBidding node sellerActor terms
    BidderBuys auctionName actor -> do
      node <- getNode
      -- FIXME: proper error printing
      Just terms <- readAuctionTerms auctionName
      bidderBuys node actor terms
    SellerReclaims auctionName -> do
      node <- getNode
      -- FIXME: proper error printing
      Just (CLIEnhancedAuctionTerms {terms, sellerActor}) <- readCLIEnhancedAuctionTerms auctionName
      sellerReclaims node sellerActor terms

prettyPrintUtxo :: (Foldable t, Show a) => t a -> IO ()
prettyPrintUtxo utxo = do
  putStrLn "Utxos: \n"
  -- FIXME print properly
  forM_ (toList utxo) $ \x ->
    putStrLn $ show x

getNode :: IO RunningNode
getNode =
  pure $
    RunningNode
      { nodeSocket = "./node.socket"
      , networkId = Testnet $ NetworkMagic 42
      }
