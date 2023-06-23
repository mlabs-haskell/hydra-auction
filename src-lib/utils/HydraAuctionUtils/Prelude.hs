{- |
Prelude for off-chain modules.

Includes imports not giving any name-collisions for:

* Basic containers types and utilities
* Our MTL-like stack and some basic functions lifted
-}
module HydraAuctionUtils.Prelude (module X, trySome, hush) where

-- Prelude imports
import Prelude as X hiding (getLine, print, putStr, putStrLn)

-- Haskell imports

import Control.Concurrent as X (
  modifyMVar_,
  threadDelay,
 )
import Control.Exception (SomeException)
import Control.Monad as X (
  forM_,
  forever,
  guard,
  replicateM_,
  void,
  when,
  (<=<),
  (>=>),
 )
import Control.Monad.Base as X (MonadBase (..))
import Control.Monad.Catch as X (
  MonadCatch (..),
  MonadMask (..),
  MonadThrow (..),
  try,
 )
import Control.Monad.Reader as X (MonadReader (..), ReaderT (..), local)
import Control.Monad.State as X (
  MonadState (get, put),
  StateT (..),
  evalStateT,
 )
import Control.Monad.Trans as X (MonadIO (..), MonadTrans (..))
import Control.Monad.Trans.Control as X (
  MonadBaseControl (..),
  MonadTransControl (..),
 )
import Data.Functor.Contravariant as X (Contravariant (..))
import Data.Kind as X (Constraint, Type)
import Data.Map as X (Map)
import Data.Maybe as X (fromJust)
import GHC.Generics as X (Generic)
import GHC.Stack as X (HasCallStack)
import Relude.Lifted.Concurrent as X
import Relude.Lifted.Handle as X
import Relude.Lifted.Terminal as X

trySome ::
  forall a m. (MonadIO m, MonadCatch m) => m a -> m (Either SomeException a)
trySome = try

-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
