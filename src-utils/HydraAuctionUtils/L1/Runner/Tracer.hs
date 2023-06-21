module HydraAuctionUtils.L1.Runner.Tracer (
  StateDirectory (..),
  HydraAuctionLog (..),
  fileTracer,
  stdoutOrNullTracer,
  showLogsOnFailure,
) where

-- Prelude imports
import Hydra.Prelude (
  IOMode (ReadWriteMode),
  TVar,
  atomically,
  onException,
  withFile,
 )
import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Concurrent.STM.TVar (modifyTVar, newTVarIO, readTVarIO)
import Control.Tracer (Tracer (..))
import Data.Aeson (FromJSON, ToJSON)
import System.FilePath ((</>))

-- Hydra imports
import Hydra.Logging (
  Verbosity,
  withTracer,
  withTracerOutputTo,
 )

{- HLINT ignore "Use newtype instead of data" -}
data HydraAuctionLog
  = FromHydraAuction String
  deriving stock (Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Show HydraAuctionLog where
  show (FromHydraAuction message) = message

-- | @FilePath@ used to store the running node data.
newtype StateDirectory = MkStateDirectory
  {stateDirectory :: FilePath}

{- | Filter tracer which logs into a `test.log` file within the given
 @StateDirectory@.
-}
fileTracer :: StateDirectory -> IO (Tracer IO HydraAuctionLog)
fileTracer MkStateDirectory {stateDirectory} = do
  withFile (stateDirectory </> "test.log") ReadWriteMode $ \h ->
    withTracerOutputTo h "Tracer" pure

-- | Stdout or null tracer depending the given verbosity level.
stdoutOrNullTracer :: Verbosity -> IO (Tracer IO HydraAuctionLog)
stdoutOrNullTracer verbosity =
  withTracer verbosity pure

-- Capture logs and output them to stdout when an exception was raised by the
-- given 'action'.
-- Copied from Hydra and simplified.
showLogsOnFailure ::
  (Show msg) =>
  (Tracer IO msg -> IO a) ->
  IO a
showLogsOnFailure action = do
  tvar <- newTVarIO []
  action (traceInTVar tvar)
    `onException` (readTVarIO tvar >>= mapM_ (putStrLn . show) . reverse)

traceInTVar ::
  TVar IO [msg] ->
  Tracer IO msg
traceInTVar tvar = Tracer $ \msg -> do
  atomically $ modifyTVar tvar (msg :)
