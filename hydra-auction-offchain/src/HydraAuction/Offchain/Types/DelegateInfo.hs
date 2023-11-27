module HydraAuction.Offchain.Types.DelegateInfo (
  DelegateInfo (..),
  DelegateInfo'Error (..),
  validateDelegateInfo,
) where

import GHC.Generics (Generic)
import Prelude

import Data.Text (Text)
import Data.Validation (Validation)

import HydraAuction.Error.Types.DelegateInfo (
  DelegateInfo'Error (..),
 )
import HydraAuction.Offchain.Lib.Crypto (
  Hash,
  PaymentKey,
 )
import HydraAuction.Offchain.Lib.Validation (err)

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
