module HydraAuction.Offchain.Types.AuctionInfo (
  AuctionInfo (..),
  validateAuctionInfo,
  fromPlutusAuctionInfo,
  toPlutusAuctionInfo,
) where

import GHC.Generics (Generic)
import Prelude

import Data.Function ((&))
import Data.Validation (Validation)

import Cardano.Api.Shelley (
  AddressInEra,
  BabbageEra,
  NetworkId,
  PolicyId,
 )

import HydraAuction.Error.Types.AuctionInfo (AuctionInfo'Error (..))
import HydraAuction.Offchain.Lib.Codec.Onchain (
  fromPlutusAddress,
  fromPlutusPolicyId,
  toPlutusAddress,
  toPlutusPolicyId,
 )
import HydraAuction.Offchain.Lib.Validation (errWith)
import HydraAuction.Offchain.Types.AuctionTerms (
  AuctionTerms (..),
  fromPlutusAuctionTerms,
  toPlutusAuctionTerms,
  validateAuctionTerms,
 )

import HydraAuction.Onchain.Types.AuctionInfo qualified as O

type AuctionAddress = AddressInEra BabbageEra

data AuctionInfo = AuctionInfo
  { ai'AuctionCs :: !PolicyId
  -- ^ The auction is uniquely identified by
  -- the currency symbol of its state tokens.
  , ai'AuctionTerms :: !AuctionTerms
  -- ^ The auction terms fully characterize the
  -- behaviour of the auction.
  , ai'AuctionEscrow :: AuctionAddress
  , ai'BidderDeposit :: AuctionAddress
  , ai'FeeEscrow :: AuctionAddress
  , ai'StandingBid :: AuctionAddress
  }
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Validation
-- -------------------------------------------------------------------------
validateAuctionInfo ::
  AuctionInfo ->
  Validation [AuctionInfo'Error] ()
validateAuctionInfo AuctionInfo {..} =
  --
  -- The auction terms in the metadata record should be valid.
  validateAuctionTerms ai'AuctionTerms
    `errWith` AuctionInfo'Error'InvalidAuctionTerms

-- -------------------------------------------------------------------------
-- Conversion to onchain
-- -------------------------------------------------------------------------
toPlutusAuctionInfo :: AuctionInfo -> O.AuctionInfo
toPlutusAuctionInfo AuctionInfo {..} =
  O.AuctionInfo
    { O.ai'AuctionCs =
        ai'AuctionCs & toPlutusPolicyId
    , --
      O.ai'AuctionTerms =
        ai'AuctionTerms & toPlutusAuctionTerms
    , --
      O.ai'AuctionEscrow =
        ai'AuctionEscrow & toPlutusAddress
    , --
      O.ai'BidderDeposit =
        ai'BidderDeposit & toPlutusAddress
    , --
      O.ai'FeeEscrow =
        ai'FeeEscrow & toPlutusAddress
    , --
      O.ai'StandingBid =
        ai'StandingBid & toPlutusAddress
    }

-- -------------------------------------------------------------------------
-- Conversion from onchain
-- -------------------------------------------------------------------------
fromPlutusAuctionInfo :: NetworkId -> O.AuctionInfo -> Maybe AuctionInfo
fromPlutusAuctionInfo net O.AuctionInfo {..} = do
  m'ai'AuctionCs <-
    ai'AuctionCs & fromPlutusPolicyId
  --
  m'ai'AuctionTerms <-
    ai'AuctionTerms & fromPlutusAuctionTerms
  --
  m'ai'AuctionEscrow <-
    ai'AuctionEscrow & fromPlutusAddress net
  --
  m'ai'BidderDeposit <-
    ai'BidderDeposit & fromPlutusAddress net
  --
  m'ai'FeeEscrow <-
    ai'FeeEscrow & fromPlutusAddress net
  --
  m'ai'StandingBid <-
    ai'StandingBid & fromPlutusAddress net
  --
  pure $
    AuctionInfo
      { ai'AuctionCs = m'ai'AuctionCs
      , ai'AuctionTerms = m'ai'AuctionTerms
      , ai'AuctionEscrow = m'ai'AuctionEscrow
      , ai'BidderDeposit = m'ai'BidderDeposit
      , ai'FeeEscrow = m'ai'FeeEscrow
      , ai'StandingBid = m'ai'StandingBid
      }
