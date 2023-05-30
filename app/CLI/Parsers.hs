module CLI.Parsers (
  getCliInput,
  parseCliAction,
  CliInput (..),
  CliOptions (..),
  PromptOptions (..),
) where

-- Prelude imports

import Cardano.Prelude (asum)
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
import Options.Applicative.Builder (
  ReadM,
  eitherReader,
  option,
 )

-- Cardano node imports
import CardanoNode (RunningNode (..))

-- Hydra imports
import Hydra.Network (Host)

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuctionUtils.Fixture (Actor (..))
import HydraAuctionUtils.Parsers (
  cardanoRunningNodeParser,
  parseActor,
  parseAda,
  parseHost,
 )
import HydraAuctionUtils.Types.Natural (Natural)

-- Hydra auction CLI imports
import CLI.Actions (CliAction (..), Layer (..), seedAmount)
import CLI.Config (AuctionName (..))

data CliInput = CliInput
  { cliOptions :: CliOptions
  , delegateSettings :: Host
  }

data CliOptions
  = Watch !AuctionName
  | InteractivePrompt !PromptOptions

data PromptOptions = MkPromptOptions
  { cliActor :: !Actor
  , cliVerbosity :: !Bool
  , cliCardanoNode :: !RunningNode
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
        <$> (MkPromptOptions <$> actor <*> verboseParser <*> cardanoRunningNodeParser)
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
      , command "show-actors-with-min-deposit" (info (ShowActorsMinDeposit <$> auctionName <*> depositAmount) (progDesc "Show actors that deposited at least DEPOSIT_AMOUNT"))
      , command "seed" (info (pure Seed) (progDesc $ "Provides " <> show seedAmount <> " Lovelace for the given actor"))
      , command "prepare-for-demo" (info (Prepare <$> actor) (progDesc $ "Provides " <> show seedAmount <> " Lovelace for every actor and 1 Test NFT for given actor"))
      , command "mint-test-nft" (info (pure MintTestNFT) (progDesc "Mints an NFT that can be used as auction lot"))
      , command "announce-auction" (info (AuctionAnounce <$> auctionName) (progDesc "Create an auction"))
      , command "start-bidding" (info (StartBidding <$> auctionName) (progDesc "Open an auction for bidding"))
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

delegate :: Parser Host
delegate =
  option
    (parseHost Nothing)
    ( short 'd'
        <> metavar "DELEGATE"
        <> help "Host and port of delegate server"
    )

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

depositAmount :: Parser Natural
depositAmount =
  option
    parseAda
    ( short 'b'
        <> metavar "DEPOSIT_AMOUNT"
        <> help "Deposit amount"
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

parseScript :: ReadM AuctionScript
parseScript = eitherReader $ \case
  "escrow" -> pure Escrow
  "standing-bid" -> pure StandingBid
  "fee-escrow" -> pure FeeEscrow
  _ -> Left "Escrow parsing error"

verboseParser :: Parser Bool
verboseParser = switch (long "verbose" <> short 'v')
