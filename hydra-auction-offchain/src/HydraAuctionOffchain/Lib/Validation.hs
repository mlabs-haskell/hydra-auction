module HydraAuctionOffchain.Lib.Validation (
  err,
  errWith,
) where

import Prelude

import Data.Bifunctor
import Data.Validation (Validation (..))

err :: Bool -> e -> Validation [e] ()
err x e = if x then Success () else Failure [e]

errWith :: Validation [e1] a -> (e1 -> e2) -> Validation [e2] a
errWith v f = first (fmap f) v
