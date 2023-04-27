module EndToEnd.Utils (
  mkAssertion,
  config,
  assertNFTNumEquals,
  assertUTxOsInScriptEquals,
) where

-- Prelude imports

import PlutusTx.Prelude

-- Cardano node imports
import Cardano.Api.UTxO qualified as UTxO

-- Haskell imports
import Control.Monad.Trans (MonadIO (..))

-- Haskell test imports

import Data.Maybe (fromJust)
import Test.Hydra.Prelude (failAfter)
import Test.Tasty.HUnit (Assertion, (@=?), (@?=))

-- Plutus imports
import Plutus.V1.Ledger.Value (assetClassValueOf)

-- Hydra imports
import Hydra.Cardano.Api (toPlutusValue, txOutValue)

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.OnChain.TestNFT (testNftAssetClass)
import HydraAuction.Tx.Common (scriptUtxos)
import HydraAuction.Tx.TermsConfig (AuctionTermsConfig (..))
import HydraAuction.Types (AuctionTerms (..), intToNatural)
import HydraAuctionUtils.Fixture (Actor)
import HydraAuctionUtils.L1.Runner (L1Runner, executeTestL1Runner, withActor)
import HydraAuctionUtils.Monads.Actors (actorTipUtxo)

config :: AuctionTermsConfig
config =
  AuctionTermsConfig
    { configDiffBiddingStart = 2
    , configDiffBiddingEnd = 5
    , configDiffVoucherExpiry = 8
    , configDiffCleanup = 10
    , configAuctionFeePerDelegate = fromJust $ intToNatural 4_000_000
    , configStartingBid = fromJust $ intToNatural 8_000_000
    , configMinimumBidIncrement = fromJust $ intToNatural 8_000_000
    }

mkAssertion :: L1Runner () -> Assertion
mkAssertion = failAfter 60 . executeTestL1Runner

assertNFTNumEquals :: Actor -> Integer -> L1Runner ()
assertNFTNumEquals actor expectedNum = do
  utxo <- withActor actor actorTipUtxo
  liftIO $ do
    let value =
          mconcat
            [toPlutusValue $ txOutValue out | (_, out) <- UTxO.pairs utxo]
    assetClassValueOf value testNftAssetClass @=? expectedNum

assertUTxOsInScriptEquals :: AuctionScript -> AuctionTerms -> Integer -> L1Runner ()
assertUTxOsInScriptEquals script terms expectedNum = do
  utxo <- scriptUtxos script terms
  liftIO $ length (UTxO.pairs utxo) @?= expectedNum
