{-# OPTIONS -Wno-orphans #-}
module CLI.Types (
  CLIError (..),
  CLILog (..),
  Layer (..),
  CliAction (..),
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Prettyprinter (Doc, Pretty (pretty), indent, line)

-- Hydra imports

import Hydra.Cardano.Api (Lovelace)
import Hydra.Cluster.Faucet (Marked (..))

-- Hydra auction imports

import HydraAuction.OnChain (AuctionScript (..))
import HydraAuctionUtils.Fixture (Actor)
import HydraAuctionUtils.Types.Natural (Natural)

-- Hydra auction CLI imports
import CLI.Config (AuctionName)

{- HLINT ignore "Use newtype instead of data" -}
data CLILog
  = CLIError CLIError

{- HLINT ignore "Use newtype instead of data" -}
data CLIError
  = InvalidDelegateResponse String

instance Pretty CLIError where
  pretty = \case
    InvalidDelegateResponse str ->
      "Delegate gave an invalid response:"
        <> extraInfo (pretty str)

-- | additional information on a log event
extraInfo :: forall ann. Doc ann -> Doc ann
extraInfo = (line <>) . indent 2

data Layer = L1 | L2 deriving stock (Show)

-- FIXME: upstream
deriving stock instance Show Marked

data CliAction
  = ShowCurrentStage !AuctionName
  | ShowAddress
  | ShowScriptUtxos !AuctionName !AuctionScript
  | ShowUtxos
  | ShowAllUtxos
  | ShowCurrentWinningBidder !AuctionName
  | ShowActorsMinDeposit !AuctionName !Natural
  | Seed
  | Prepare !Actor
  | TransferAda !Actor !Marked !Lovelace
  | MintTestNFT
  | AuctionAnounce !AuctionName
  | MakeDeposit !AuctionName !Natural
  | StartBidding !AuctionName
  | MoveToL2 !AuctionName
  | NewBid !AuctionName !Natural !Layer
  | BidderBuys !AuctionName
  | BidderClaimsDeposit !AuctionName
  | SellerReclaims !AuctionName
  | SellerClaimsDepositFor !AuctionName !Actor
  | SubmitSignatureToPlatform !AuctionName !Actor
  | Cleanup !AuctionName
  | CleanupDeposit !AuctionName
  deriving stock (Show)
