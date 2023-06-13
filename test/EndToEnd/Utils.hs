module EndToEnd.Utils (
  mkAssertion,
  mkAssertionOfIO,
  config,
  lookupBoolEnv,
  EnvParam,
  assertNFTNumEquals,
  assertUTxOsInScriptEquals,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Cardano node imports
import Cardano.Api.UTxO qualified as UTxO

-- Haskell imports
import Control.Exception (SomeException, throw)
import System.Environment (lookupEnv)
import System.IO.Silently (capture)

-- Haskell test imports
import Test.Hydra.Prelude (failAfter)
import Test.Tasty.HUnit (Assertion, (@=?), (@?=))

-- Plutus imports
import PlutusLedgerApi.V1.Value (assetClassValueOf)

-- Hydra imports
import Hydra.Cardano.Api (
  toPlutusValue,
  txOutValue,
 )

-- Hydra auction imports

import HydraAuction.OnChain (AuctionScript)
import HydraAuction.OnChain.TestNFT (testNftAssetClass)
import HydraAuction.Tx.Common (scriptUtxos)
import HydraAuction.Tx.TermsConfig (AuctionTermsConfig (..))
import HydraAuction.Types (AuctionTerms (..))
import HydraAuctionUtils.Fixture (Actor)
import HydraAuctionUtils.L1.Runner (L1Runner, executeTestL1Runner, withActor)
import HydraAuctionUtils.Monads.Actors (actorTipUtxo)
import HydraAuctionUtils.Types.Natural (intToNatural)

config :: AuctionTermsConfig
config =
  AuctionTermsConfig
    { configDiffBiddingStart = 2
    , configDiffBiddingEnd = 5
    , configDiffVoucherExpiry = 8
    , configDiffCleanup = 10
    , configAuctionFeePerDelegate = fromJust $ intToNatural 4_000_000
    , configStartingBid = fromJust $ intToNatural 15_000_000
    , configMinimumBidIncrement = fromJust $ intToNatural 10_000_000
    }

-- FIXME: move to separate module
data EnvParam = Verbose

lookupBoolEnv :: EnvParam -> IO Bool
lookupBoolEnv Verbose = do
  -- FIXME: better and unified parsing of envs,
  -- report them to user before tests execution
  mVerboseStr <- lookupEnv "TESTS_VERBOSE"
  return $ mVerboseStr == Just "1"

autoCaptureStdout :: forall b. HasCallStack => IO b -> IO b
autoCaptureStdout action = do
  verboseMode <- lookupBoolEnv Verbose
  if verboseMode then action else capturingAction
  where
    capturingAction = do
      (captured, mResult) <- capture $ try action
      case mResult of
        Right result -> return result
        Left (exception :: SomeException) -> do
          printCaptured captured
          throw exception
    printCaptured captured = putStrLn $ "Captured stdout: \n" <> captured

-- FIXME: autoCaptureStdout eats Tasty output as well
-- FIXME: shorter timeout
mkAssertionOfIO :: IO () -> Assertion
mkAssertionOfIO = autoCaptureStdout . failAfter 120

mkAssertion :: L1Runner () -> Assertion
mkAssertion = mkAssertionOfIO . executeTestL1Runner

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
  liftIO $ length (UTxO.pairs utxo) @?= fromInteger expectedNum
