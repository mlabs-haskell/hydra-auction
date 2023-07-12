module HydraAuctionUtils.Types (Layer (..)) where

import HydraAuctionUtils.Prelude

import Data.Aeson (FromJSON, ToJSON)

-- Common types

data Layer = L1 | L2
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
