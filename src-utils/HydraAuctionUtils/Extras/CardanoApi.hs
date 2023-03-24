module HydraAuctionUtils.Extras.CardanoApi (
    networkIdToNetwork
    ) where

-- Cardano ledger imports
import Cardano.Ledger.BaseTypes qualified as Cardano

-- Hydra imports
import Hydra.Cardano.Api (NetworkId (..))

networkIdToNetwork :: NetworkId -> Cardano.Network
networkIdToNetwork (Testnet _) = Cardano.Testnet
networkIdToNetwork Mainnet = Cardano.Mainnet
