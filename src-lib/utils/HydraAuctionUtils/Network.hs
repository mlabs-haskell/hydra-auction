module HydraAuctionUtils.Network (withClientForHost) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Monad.Trans.Control (liftBaseOp)
import Data.Text qualified as Text
import Network.WebSockets (Connection, runClient)

-- Hydra imports

import Hydra.Network (Host (..))

withClientForHost ::
  forall x m. MonadBaseControl IO m => Host -> Text.Text -> (Connection -> m x) -> m x
withClientForHost settings path =
  liftBaseOp runActionInIO
  where
    runActionInIO =
      runClient
        (Text.unpack $ hostname settings)
        (fromIntegral $ port settings)
        (Text.unpack path)
