module HydraAuctionUtils.Parsers (
  parseActor,
  parseAda,
  parseNetworkMagic,
  parseHost,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Data.Char (toLower)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Network.HostAndPort (maybeHostAndPort)
import Options.Applicative.Builder (ReadM, eitherReader)
import Protolude.Exceptions (note)
import Text.Read (readMaybe)

-- Cardano node imports
import Cardano.Api (NetworkMagic (..))

-- Hydra imports
import Hydra.Network (Host (..))

-- HydraAuction imports
import HydraAuctionUtils.Fixture (Actor (..), actorName)
import HydraAuctionUtils.Types.Natural (Natural, intToNatural)

parseActor :: ReadM Actor
parseActor =
  eitherReader $
    note "failed to parse actor" . parseToMaybe
  where
    parseToMaybe = flip Map.lookup nameToActor . fmap toLower
    nameToActor =
      Map.fromList
        [ (actorName actor, actor)
        | actor <- [minBound .. maxBound]
        ]

-- FIXME: use Lovelace type
parseAda :: ReadM Natural
parseAda = eitherReader $ \s -> note "failed to parse Ada" $ do
  ada <- readMaybe s
  let lovelace = ada * 1_000_000
  intToNatural lovelace

parseNetworkMagic :: ReadM NetworkMagic
parseNetworkMagic = eitherReader $ \s -> note "failed to parse network magic" $ do
  magic <- readMaybe s
  pure $ NetworkMagic magic

parseHost :: Maybe Int -> ReadM Host
parseHost defaultPort =
  -- FIXME: custom error in case of port missing but requied
  eitherReader $ \s -> note "failed to parse host and port" $
    do
      (host, mPortString) <- maybeHostAndPort s
      portString <- mPortString <> (show <$> defaultPort)
      port <- readMaybe portString
      return $ Host (Text.pack host) port
