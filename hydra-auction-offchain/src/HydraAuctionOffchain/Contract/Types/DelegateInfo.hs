module HydraAuctionOffchain.Contract.Types.DelegateInfo (
  DelegateInfo (..),
  DelegateInfo'Error (..),
  validateDelegateInfo,
) where

import GHC.Generics (Generic)
import Prelude

import Data.Text (Text)
import Data.Validation (Validation)

import HydraAuctionOffchain.Lib.Crypto (
  Hash,
  PaymentKey,
 )
import HydraAuctionOffchain.Lib.Validation (err)

import HydraAuctionOffchain.Contract.Types.DelegateInfoError (
  DelegateInfo'Error (..),
 )

data DelegateInfo = DelegateInfo
  { di'GroupName :: Text
  , di'GroupURL :: Text
  , di'Delegates :: [Hash PaymentKey]
  -- ^ The payment vkey hashes of the individual delegates in this group.
  -- These payment vkey hashes also indicate where the delegates
  -- should receive payment for their services.
  }
  deriving stock (Eq, Generic, Show)

-- -------------------------------------------------------------------------
-- Validation
-- -------------------------------------------------------------------------

validateDelegateInfo ::
  DelegateInfo ->
  Validation [DelegateInfo'Error] ()
validateDelegateInfo DelegateInfo {..} =
  --
  -- (DI01) There must be at least one delegate.
  (length di'Delegates > 0)
    `err` DelegateInfo'Error'NoDelegates
