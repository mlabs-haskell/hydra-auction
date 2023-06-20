{-# OPTIONS -Wno-orphans #-}
module CLI.Types (
  CLIError (..),
  CLILog (..),
  Layer (..),
  CliAction (..),
  PerAuctionCliAction (..),
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
  = ShowAddress
  | ShowUtxos
  | ShowAllUtxos
  | ShowScriptInfo
  | Seed
  | Prepare Actor
  | TransferAda Actor Marked Lovelace
  | MintTestNFT
  | PerAuction AuctionName PerAuctionCliAction
  deriving stock (Show)

data PerAuctionCliAction
  = ShowCurrentStage
  | ShowScriptUtxos AuctionScript
  | ShowCurrentWinningBidder
  | ShowActorsMinDeposit Natural
  | AuctionAnounce
  | MakeDeposit Lovelace
  | ApproveBidder Actor
  | StartBidding
  | MoveToL2
  | NewBid Natural Layer
  | BidderBuys
  | BidderClaimsDeposit
  | SellerReclaims
  | Cleanup
  | CleanupDeposit
  deriving stock (Show)
