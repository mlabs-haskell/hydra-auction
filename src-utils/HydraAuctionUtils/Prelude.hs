{- |
Prelude for off-chain modules.

Includes imports not giving any name-collisions for:

* Basic containers types and utilities
* Our MTL-like stack
-}
module HydraAuctionUtils.Prelude (module X) where

-- Prelude imports
import Prelude as X

-- Haskell imports

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
import Control.Monad.Catch as X (
  MonadCatch (..),
  MonadMask (..),
  MonadThrow (..),
  try,
 )
import Control.Monad.Reader as X (MonadReader (..), ReaderT (..), local)
import Control.Monad.State as X (MonadState (..), StateT (..), evalStateT)
import Control.Monad.Trans as X (MonadIO (..), MonadTrans (..))
import Data.Functor.Contravariant as X (Contravariant (..))
import Data.Kind as X (Constraint, Type)
import Data.Map as X (Map)
import Data.Maybe as X (fromJust)
import GHC.Generics as X (Generic)
import GHC.Stack as X (HasCallStack)
