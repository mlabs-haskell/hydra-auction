{-# OPTIONS -Wno-orphans #-}
module HydraAuctionUtils.Extras.CardanoApi (
  networkIdToNetwork,
  Lovelace (..),
) where

-- Prelude imports
import PlutusTx.Prelude

-- Cardano ledger imports
import Cardano.Ledger.BaseTypes qualified as Cardano

-- Hydra imports
import Hydra.Cardano.Api (Lovelace (..), NetworkId (..))

networkIdToNetwork :: NetworkId -> Cardano.Network
networkIdToNetwork (Testnet _) = Cardano.Testnet
networkIdToNetwork Mainnet = Cardano.Mainnet

deriving newtype instance MultiplicativeSemigroup Lovelace
deriving newtype instance MultiplicativeMonoid Lovelace
deriving newtype instance AdditiveSemigroup Lovelace
deriving newtype instance AdditiveMonoid Lovelace
deriving newtype instance AdditiveGroup Lovelace
