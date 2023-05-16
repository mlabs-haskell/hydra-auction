{-# LANGUAGE ApplicativeDo #-}

module DelegateServer.Parsers (delegateConfigParser) where

-- Prelude imports
import Prelude

-- Haskell imports
import Options.Applicative (Parser)
import Options.Applicative.Builder (help, long, metavar, option)

-- Hydra imports
import Hydra.Network (Host)

-- Hydra auction imports

import HydraAuction.Delegate.Server (DelegateServerConfig (..))
import HydraAuctionUtils.Fixture (Actor)
import HydraAuctionUtils.Parsers (
  cardanoRunningNodeParser,
  parseActor,
  parseHost,
 )

delegateConfigParser :: Parser DelegateServerConfig
delegateConfigParser = do
  l1Actor <- actorParser
  cardanoNode <- cardanoRunningNodeParser
  websocketsHost <- websocketsHostParser
  hydraNodeHost <- hydraNodeParser
  return
    DelegateServerConfig
      { websocketsHost
      , cardanoNode
      , hydraNodeHost
      , l1Actor
      , tick = tick
      , ping = ping
      }
  where
    tick :: Int
    tick = 1_000
    ping :: Int
    ping = 30

actorParser :: Parser Actor
actorParser =
  option
    parseActor
    ( long "actor"
        <> metavar "ACTOR"
        <> help "Actor matching Hydra Node we working with"
    )

hydraNodeParser :: Parser Host
hydraNodeParser =
  option
    (parseHost Nothing)
    ( long "hydra-node"
        <> metavar "HYDRA_NODE"
        <> help "Host and port of Hydra node to work with"
    )

websocketsHostParser :: Parser Host
websocketsHostParser =
  option
    (parseHost Nothing)
    ( long "websockets-host"
        <> metavar "WEBSOCKETS_HOST"
        <> help "Host and port to use for serving Websocket server"
    )
