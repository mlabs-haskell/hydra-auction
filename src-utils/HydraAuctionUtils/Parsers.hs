module HydraAuctionUtils.Parsers (
  parseActor,
  parseAda,
  parseAdaAsNatural,
  parseMarked,
  parseNetworkMagic,
  parseHost,
  cardanoRunningNodeParser,
  execParserForCliArgs,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports

import Control.Applicative ((<**>))
import Data.Char (toLower)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Network.HostAndPort (maybeHostAndPort)
import Options.Applicative (Parser, customExecParser, helper)
import Options.Applicative.Builder (ReadM, eitherReader, fullDesc, help, info, long, metavar, option, prefs, showHelpOnEmpty, showHelpOnError, strOption)
import Protolude.Exceptions (note)
import Text.Read (readMaybe)

-- Cardano node imports

import Cardano.Api (NetworkId, NetworkMagic (..), fromNetworkMagic)
import CardanoNode (RunningNode (..))
import Hydra.Cardano.Api (Lovelace (..))
import Hydra.Cluster.Faucet (Marked (..))

-- Hydra imports
import Hydra.Network (Host (..))

-- HydraAuction imports
import HydraAuctionUtils.Fixture (Actor (..), actorName)
import HydraAuctionUtils.Types.Natural (Natural, intToNatural, naturalToInt)

execParserForCliArgs :: forall x. Parser x -> IO x
execParserForCliArgs parser = do
  customExecParser preferences options
  where
    options =
      info
        (parser <**> helper)
        fullDesc
    preferences = prefs (showHelpOnEmpty <> showHelpOnError)

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

-- FIXME: remove
parseAdaAsNatural :: ReadM Natural
parseAdaAsNatural = eitherReader $ \s -> note "failed to parse Ada" $ do
  ada <- readMaybe s
  let lovelace = ada * 1_000_000
  intToNatural lovelace

parseAda :: ReadM Lovelace
parseAda = Lovelace . naturalToInt <$> parseAdaAsNatural

parseMarked :: ReadM Marked
parseMarked = eitherReader $ \case
  "fuel" -> Right Fuel
  "normal" -> Right Normal
  _ -> Left "Marked parsing error"

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

cardanoRunningNodeParser :: Parser RunningNode
cardanoRunningNodeParser =
  RunningNode <$> nodeSocketParser <*> networkIdParser
  where
    nodeSocketParser :: Parser String
    nodeSocketParser =
      strOption
        ( long "cardano-node-socket"
            <> metavar "CARDANO_NODE_SOCKET"
            <> help "Absolute path to the cardano node socket"
        )

    networkIdParser :: Parser NetworkId
    networkIdParser =
      fromNetworkMagic
        <$> option
          parseNetworkMagic
          ( long "network-magic"
              <> metavar "NETWORK_MAGIC"
              <> help "Network magic for cardano"
          )
