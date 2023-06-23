module CLI.Parsers (
  getCliInput,
  parseCliAction,
  CliInput (..),
  CliOptions (..),
  PromptOptions (..),
) where

-- Prelude imports

import Cardano.Prelude (asum)
import HydraAuctionUtils.Prelude

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
  metavar,
  optional,
  prefs,
  progDesc,
  renderFailure,
  short,
  showHelpOnEmpty,
  showHelpOnError,
  strOption,
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
import Hydra.Cardano.Api (Lovelace)
import Hydra.Cluster.Faucet (Marked (..))
import Hydra.Network (Host)

-- Hydra auction imports
import HydraAuction.OnChain (AuctionScript (..))
import HydraAuctionUtils.Fixture (Actor (..))
import HydraAuctionUtils.Parsers (
  cardanoRunningNodeParser,
  parseActor,
  parseAda,
  parseAdaAsNatural,
  parseHost,
  parseMarked,
 )
import HydraAuctionUtils.Types.Natural (Natural)

-- Hydra auction CLI imports

import CLI.Actions (Layer (..), seedAmount)
import CLI.Config (AuctionName (..))
import CLI.Types (CliAction (..), PerAuctionCliAction (..))

data CliInput = CliInput
  { cliOptions :: CliOptions
  , delegateSettings :: Host
  , cliCardanoNode :: RunningNode
  }

-- FIXME: rename along with CliAction
data CliOptions
  = Watch AuctionName
  | InteractivePrompt PromptOptions

data PromptOptions = MkPromptOptions
  { cliActor :: Actor
  , cliNoninteractiveAction :: Maybe String
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
cliInputParser =
  CliInput <$> cliOptionsParser <*> delegate <*> cardanoRunningNodeParser

cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  asum
    [ Watch <$> watchAuction
    , InteractivePrompt
        <$> (MkPromptOptions <$> actor <*> action)
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
      [ command "show-script-utxos" (info (perAuction $ ShowScriptUtxos <$> script) (progDesc "Show utxos at a given script. Requires the seller and auction lot for the given script"))
      , command "show-utxos" (info (pure ShowUtxos) (progDesc "Shows utxos for a given actor"))
      , command
          "show-current-stage"
          ( info
              (perAuction $ pure ShowCurrentStage)
              (progDesc "Show current auction stage - which depends on time since auction announcement")
          )
      , command "show-all-utxos" (info (pure ShowAllUtxos) (progDesc "Shows utxos for all actors"))
      , command "show-current-winner-bidder" (info (perAuction $ pure ShowCurrentWinningBidder) (progDesc "Show current winning bidder for auction"))
      , command "show-actors-with-min-deposit" (info (perAuction $ ShowActorsMinDeposit <$> depositAmount) (progDesc "Show actors that deposited at least DEPOSIT_AMOUNT"))
      , command "show-address" (info (pure ShowAddress) (progDesc "Show address of current actor"))
      , command "show-script-info" (info (pure ShowScriptInfo) (progDesc "Show scripts hashes and lenghts"))
      , command "seed" (info (pure Seed) (progDesc $ "Provides " <> show seedAmount <> " Lovelace for the given actor"))
      , command "prepare-for-demo" (info (Prepare <$> actor) (progDesc $ "Provides " <> show seedAmount <> " Lovelace for every actor and 1 Test NFT for given actor"))
      , command "transfer-ada" (info (TransferAda <$> actor <*> marked <*> amount) (progDesc "Tranfer ADA to other actor"))
      , command "mint-test-nft" (info (pure MintTestNFT) (progDesc "Mints an NFT that can be used as auction lot"))
      , command "announce-auction" (info (perAuction $ pure AuctionAnounce) (progDesc "Create an auction"))
      , command "approve-bidder" (info (perAuction $ ApproveBidder <$> actor) (progDesc "Approve actor for bidding"))
      , command "make-deposit" (info (perAuction $ MakeDeposit <$> amount) (progDesc "Put a deposit"))
      , command "start-bidding" (info (perAuction $ pure StartBidding) (progDesc "Open an auction for bidding"))
      , command "move-to-l2" (info (perAuction $ pure MoveToL2) (progDesc "Move Standing bid to L2"))
      , command "new-bid" (info (perAuction $ NewBid <$> bidAmount <*> pure L1) (progDesc "Actor places new bid after bidding is started"))
      , command
          "new-bid-on-l2"
          ( info (perAuction $ NewBid <$> bidAmount <*> pure L2) (progDesc "Actor places new bid on L2 after Standing Bid was moved on L2")
          )
      , command "bidder-buys" (info (perAuction $ pure BidderBuys) (progDesc "Pay and recieve a lot after auction end"))
      , command "losing-bidder-reclaims-deposit" (info (perAuction $ pure BidderClaimsDeposit) (progDesc "Recieve deposit back after losing on auction"))
      , command "seller-reclaims" (info (perAuction $ pure SellerReclaims) (progDesc "Seller reclaims lot after voucher end time"))
      , command "cleanup" (info (perAuction $ pure Cleanup) (progDesc "Remove standing bid UTxO after cleanup time. Is performed by seller."))
      , command "cleanup-all-deposits" (info (perAuction $ pure CleanupDeposit) (progDesc "Return deposit to bidders after cleanup time. Is performed by anyone."))
      ]
  where
    perAuction :: Parser PerAuctionCliAction -> Parser CliAction
    perAuction = (PerAuction <$> auctionName <*>)

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
    parseAdaAsNatural
    ( short 'b'
        <> metavar "BID_AMOUNT"
        <> help "Bid amount"
    )

depositAmount :: Parser Natural
depositAmount =
  option
    parseAdaAsNatural
    ( short 'b'
        <> metavar "DEPOSIT_AMOUNT"
        <> help "Deposit amount"
    )

amount :: Parser Lovelace
amount =
  option
    parseAda
    ( short 'b'
        <> metavar "AMOUNT"
        <> help "Amount"
    )

marked :: Parser Marked
marked =
  option
    parseMarked
    ( short 'm'
        <> metavar "MARKED"
        <> help "Is transfered amount marked as Hydra Fuel"
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

action :: Parser (Maybe String)
action =
  optional $
    strOption
      ( short 'c'
          <> metavar "NON_INTERACTIVE_REPL_ACTION"
          <> help "To run REPL action non-interactive"
      )

parseScript :: ReadM AuctionScript
parseScript = eitherReader $ \case
  "escrow" -> pure Escrow
  "standing-bid" -> pure StandingBid
  "fee-escrow" -> pure FeeEscrow
  _ -> Left "Escrow parsing error"
