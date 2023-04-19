module CLI.Parsers (
  getCliInput,
  parseCliAction,
  CliInput (..),
  CliOptions (..),
  DelegateSettings (..),
  PromptOptions (..),
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Options.Applicative (
  Parser,
  ParserResult (..),
  command,
  customExecParser,
  execParserPure,
  fullDesc,
  help,
  helper,
  hsubparser,
  info,
  long,
  many,
  metavar,
  prefs,
  progDesc,
  renderFailure,
  short,
  showHelpOnEmpty,
  showHelpOnError,
  strOption,
  switch,
  (<**>),
 )

-- Cardano node imports
import Cardano.Api (NetworkId, NetworkMagic (..), fromNetworkMagic)

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuction.Types (Natural, intToNatural)
import HydraAuctionUtils.Fixture (Actor (..))

-- Hydra auction CLI imports
import CLI.Actions (CliAction (..), Layer (..), seedAmount)
import CLI.Config (AuctionName (..))
import Cardano.Prelude (asum, guard, note, readMaybe)
import Hydra.Network (IP, PortNumber)
import Options.Applicative.Builder (ReadM, eitherReader, option, showDefaultWith, value)

data CliInput = CliInput
  { cliOptions :: CliOptions
  , delegateSettings :: DelegateSettings
  }

data CliOptions
  = Watch !AuctionName
  | InteractivePrompt !PromptOptions

data PromptOptions = MkPromptOptions
  { cliActor :: !Actor
  , cliVerbosity :: !Bool
  , cliNodeSocket :: !String
  , cliNetworkId :: !NetworkId
  }

getCliInput :: IO CliInput
getCliInput = customExecParser preferences options
  where
    options =
      info
        (cliInputParser <**> helper)
        fullDesc
    preferences = prefs (showHelpOnEmpty <> showHelpOnError)

cliInputParser :: Parser CliInput
cliInputParser = CliInput <$> cliOptionsParser <*> delegate

cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  asum
    [ Watch <$> watchAuction
    , InteractivePrompt
        <$> (MkPromptOptions <$> actor <*> verboseParser <*> socketDir <*> (fromNetworkMagic <$> networkMagic))
    ]

parseCliAction :: [String] -> Either String CliAction
parseCliAction s = case execParserPure preferences options s of
  Success a -> Right a
  Failure failure -> Left $ fst $ renderFailure failure ""
  CompletionInvoked _ -> Left "completion was invoked but is not supported"
  where
    options =
      info
        (cliActionParser <**> helper)
        fullDesc
    preferences = prefs (showHelpOnEmpty <> showHelpOnError)

cliActionParser :: Parser CliAction
cliActionParser =
  hsubparser $
    mconcat
      [ command "show-script-utxos" (info (ShowScriptUtxos <$> auctionName <*> script) (progDesc "Show utxos at a given script. Requires the seller and auction lot for the given script"))
      , command "show-utxos" (info (pure ShowUtxos) (progDesc "Shows utxos for a given actor"))
      , command
          "show-current-stage"
          ( info
              (ShowCurrentStage <$> auctionName)
              (progDesc "Show current auction stage - which depends on time since auction announcement")
          )
      , command "show-all-utxos" (info (pure ShowAllUtxos) (progDesc "Shows utxos for all actors"))
      , command "show-current-winner-bidder" (info (ShowCurrentWinningBidder <$> auctionName) (progDesc "Show current winning bidder for auction"))
      , command "seed" (info (pure Seed) (progDesc $ "Provides " <> show seedAmount <> " Lovelace for the given actor"))
      , command "prepare-for-demo" (info (Prepare <$> actor) (progDesc $ "Provides " <> show seedAmount <> " Lovelace for every actor and 1 Test NFT for given actor"))
      , command "mint-test-nft" (info (pure MintTestNFT) (progDesc "Mints an NFT that can be used as auction lot"))
      , command "announce-auction" (info (AuctionAnounce <$> auctionName) (progDesc "Create an auction"))
      , command "start-bidding" (info (StartBidding <$> auctionName <*> many actor) (progDesc "Open an auction for bidding"))
      , command "move-to-l2" (info (MoveToL2 <$> auctionName) (progDesc "Move Standing bid to L2"))
      , command "new-bid" (info (NewBid <$> auctionName <*> bidAmount <*> pure L1) (progDesc "Actor places new bid after bidding is started"))
      , command
          "new-bid-on-l2"
          ( info (NewBid <$> auctionName <*> bidAmount <*> pure L2) (progDesc "Actor places new bid on L2 after Standing Bid was moved on L2")
          )
      , command "bidder-buys" (info (BidderBuys <$> auctionName) (progDesc "Pay and recieve a lot after auction end"))
      , command "seller-reclaims" (info (SellerReclaims <$> auctionName) (progDesc "Seller reclaims lot after voucher end time"))
      , command "cleanup" (info (Cleanup <$> auctionName) (progDesc "Remove standing bid UTxO after cleanup time"))
      ]

auctionName :: Parser AuctionName
auctionName =
  AuctionName
    <$> strOption
      ( short 'n'
          <> metavar "AUCTION_NAME"
          <> help "Name for saving config and dynamic params of auction"
      )

actor :: Parser Actor
actor =
  option
    parseActor
    ( short 'a'
        <> metavar "ACTOR"
        <> help "Actor running the cli tool"
    )

data DelegateSettings = DelegateSettings
  { delegateIP :: IP
  , delegatePort :: PortNumber
  }

delegate :: Parser DelegateSettings
delegate = DelegateSettings <$> dlgtIP <*> (dlgtPort <*> dlgtNumber)

dlgtIP :: Parser IP
dlgtIP =
  option parseIP $
    mconcat
      [ short 'i'
      , long "ip"
      , metavar "DELEGATE_IP"
      , help "the IP address of the delegate server"
      ]
  where
    parseIP :: ReadM IP
    parseIP = eitherReader $ note "not a valid IP address" . readMaybe

dlgtNumber :: Parser Int
dlgtNumber =
  option parseNumber $
    mconcat
      [ short 'd'
      , long "delegate-number"
      , metavar "DELEGATE_NUMBER"
      , help "the number delegate-number"
      ]
  where
    parseNumber :: ReadM Int
    parseNumber = eitherReader $ \s -> do
      num <- note "the input is not a number" $ readMaybe s
      guard (num >= 0)
      pure num

dlgtPort :: Parser (Int -> PortNumber)
dlgtPort =
  option parsePort $
    mconcat
      [ short 'p'
      , long "port"
      , metavar "DELEGATE_PORT"
      , help "the PORT of the delegate server"
      , value $ fromIntegral . (8000 +)
      , showDefaultWith (const "8000 + delegateNumber")
      ]
  where
    parsePort :: ReadM (Int -> PortNumber)
    parsePort = eitherReader $ fmap const . note "not a valid port number" . readMaybe

script :: Parser AuctionScript
script =
  option
    parseScript
    ( short 's'
        <> metavar "SCRIPT"
        <> help "Script to check. One of: escrow, standing-bid, fee-escrow"
    )

bidAmount :: Parser Natural
bidAmount =
  option
    parseAda
    ( short 'b'
        <> metavar "BID_AMOUNT"
        <> help "Bid amount"
    )

watchAuction :: Parser AuctionName
watchAuction =
  AuctionName
    <$> strOption
      ( short 'w'
          <> long "watch-auction"
          <> metavar "AUCTION"
          <> help "Watch the status of the given auction"
      )

socketDir :: Parser String
socketDir =
  strOption
    ( long "cardano-node-socket"
        <> metavar "CARDANO_NODE_SOCKET"
        <> help "Absolute path to the cardano node socket"
    )

networkMagic :: Parser NetworkMagic
networkMagic =
  option
    parseNetworkMagic
    ( long "network-magic"
        <> metavar "NETWORK_MAGIC"
        <> help "Network magic for cardano"
    )

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

parseScript :: ReadM AuctionScript
parseScript = eitherReader $ \case
  "escrow" -> pure Escrow
  "standing-bid" -> pure StandingBid
  "fee-escrow" -> pure FeeEscrow
  _ -> Left "Escrow parsing error"

parseAda :: ReadM Natural
parseAda = eitherReader $ \s -> note "failed to parse Ada" $ do
  ada <- readMaybe s
  let lovelace = ada * 1_000_000
  intToNatural lovelace

parseNetworkMagic :: ReadM NetworkMagic
parseNetworkMagic = eitherReader $ \s -> note "failed to parse network magic" $ do
  magic <- readMaybe s
  pure $ NetworkMagic magic

verboseParser :: Parser Bool
verboseParser = switch (long "verbose" <> short 'v')
