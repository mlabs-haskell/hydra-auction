module HydraAuctionUtils.Network (withClientForHost) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports

import Data.Text qualified as Text
import Network.WebSockets (Connection, runClient)

-- Hydra imports

import Hydra.Network (Host (..))

withClientForHost ::
  forall x m. MonadIO m => Host -> Text.Text -> (Connection -> IO x) -> m x
withClientForHost settings path =
  liftIO
    . runClient
      (Text.unpack $ hostname settings)
      (fromIntegral $ port settings)
      (Text.unpack path)
