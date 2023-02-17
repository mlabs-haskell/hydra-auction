module HydraAuction.Runner.Tracer (
  StateDirectory (..),
  HydraAuctionLog (..),
  fileTracer,
  stdoutTracer,
) where

-- Prelude imports
import Hydra.Prelude

-- Haskell imports
import System.FilePath ((</>))

-- Hydra imports
import Hydra.Logging (
  Tracer,
  Verbosity,
  withTracer,
  withTracerOutputTo,
 )
import HydraNode (EndToEndLog)

data HydraAuctionLog
  = FromHydra !EndToEndLog
  | FromHydraAuction !String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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

-- | Stdout tracer using the given verbosity level.
stdoutTracer :: Verbosity -> IO (Tracer IO HydraAuctionLog)
stdoutTracer verbosity =
  withTracer verbosity pure
