module HydraAuctionUtils.Parsers (
  parseActor,
  parseAda,
  parseNetworkMagic,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Options.Applicative.Builder (ReadM, eitherReader)
import Protolude.Exceptions (note)
import Text.Read (readMaybe)

-- Cardano node imports
import Cardano.Api (NetworkMagic (..))

-- HydraAuction imports
import HydraAuctionUtils.Fixture (Actor (..))
import HydraAuctionUtils.Types.Natural (Natural, intToNatural)

parseActor :: ReadM Actor
parseActor = eitherReader $ \case
  "alice" -> pure Alice
  "bob" -> pure Bob
  "carol" -> pure Carol
  "dave" -> pure Dave
  "eve" -> pure Eve
  "frank" -> pure Frank
  "grace" -> pure Grace
  "hans" -> pure Hans
  _ -> Left "Actor parsing error"

parseAda :: ReadM Natural
parseAda = eitherReader $ \s -> note "failed to parse Ada" $ do
  ada <- readMaybe s
  let lovelace = ada * 1_000_000
  intToNatural lovelace

parseNetworkMagic :: ReadM NetworkMagic
parseNetworkMagic = eitherReader $ \s -> note "failed to parse network magic" $ do
  magic <- readMaybe s
  pure $ NetworkMagic magic
