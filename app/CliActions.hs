module CliActions (
  CliAction (..),
  handleCliAction,
) where

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
import Test.Hydra.Prelude (withTempDir)

import Hydra.Logging (showLogsOnFailure)
import HydraAuction.OnChain
import HydraNode (
  EndToEndLog (FromCardanoNode, FromFaucet),
 )
import System.FilePath ((</>))
import System.IO (IOMode (ReadWriteMode), withFile)

data CliAction
  = RunCardanoNode
  | ShowScriptUtxos !AuctionScript !Actor !TxIn
  | ShowUtxos !Actor
  | Seed !Actor
  | MintTestNFT !Actor
  | AuctionAnounce !Actor !TxIn
  | StartBidding !Actor !TxIn
  | BidderBuys !Actor !TxIn
  | SellerReclaims !Actor !TxIn

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
        seedFromFaucet_ node key 100_000_000 Normal (contramap FromFaucet tracer)
    ShowScriptUtxos script actor utxoRef -> do
      node <- getNode
      terms <- constructTerms node actor utxoRef
      utxos <- scriptUtxos node script terms
      prettyPrintUtxo utxos
    ShowUtxos actor -> do
      node <- getNode
      utxos <- actorTipUtxo node actor
      prettyPrintUtxo utxos
    MintTestNFT actor -> do
      node <- getNode
      void $ mintOneTestNFT node actor
    AuctionAnounce actor utxo -> do
      node <- getNode
      terms <- constructTerms node actor utxo
      announceAuction node actor terms
    StartBidding actor utxo -> do
      node <- getNode
      terms <- constructTerms node actor utxo
      startBidding node actor terms
    BidderBuys actor utxo -> do
      node <- getNode
      terms <- constructTerms node actor utxo
      bidderBuys node actor terms
    SellerReclaims actor utxo -> do
      node <- getNode
      terms <- constructTerms node actor utxo
      sellerReclaims node actor terms

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
