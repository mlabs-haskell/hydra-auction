module HydraAuctionOffchain.Contract.Types.DelegateInfo (
  DelegateInfo (..),
  DelegateInfo'Error (..),
  validateDelegateInfo,
) where

import Prelude

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Validation (Validation)
import GHC.Generics (Generic)

import HydraAuctionOffchain.Lib.Crypto (
  Hash,
  PaymentKey,
 )
import HydraAuctionOffchain.Lib.Validation (err)

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

data DelegateInfo'Error
  = DelegateInfo'Error'NoDelegates
  deriving stock (Eq, Generic, Show)

validateDelegateInfo ::
  DelegateInfo ->
  Validation [DelegateInfo'Error] ()
validateDelegateInfo DelegateInfo {..} =
  fold
    [ (length di'Delegates > 0)
        `err` DelegateInfo'Error'NoDelegates
    ]
