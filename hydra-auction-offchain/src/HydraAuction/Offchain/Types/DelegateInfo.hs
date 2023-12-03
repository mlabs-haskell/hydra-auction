module HydraAuction.Offchain.Types.DelegateInfo (
  DelegateInfo (..),
  DelegateInfo'Error (..),
  validateDelegateInfo,
  toPlutusDelegateInfo,
) where

import GHC.Generics (Generic)
import Prelude

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Validation (Validation)

import HydraAuction.Error.Types.DelegateInfo (
  DelegateInfo'Error (..),
 )
import HydraAuction.Offchain.Lib.Codec.Onchain (
  toPlutusTextUtf8,
  toPlutusVKeyHash,
 )
import HydraAuction.Offchain.Lib.Crypto (
  Hash,
  PaymentKey,
 )
import HydraAuction.Offchain.Lib.Validation (err)

import HydraAuction.Onchain.Types.DelegateInfo qualified as O

data DelegateInfo = DelegateInfo
  { di'GroupName :: !Text
  , di'GroupURL :: !Text
  , di'Delegates :: ![Hash PaymentKey]
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

-- -------------------------------------------------------------------------
-- Conversion to onchain
-- -------------------------------------------------------------------------
toPlutusDelegateInfo :: DelegateInfo -> O.DelegateInfo
toPlutusDelegateInfo DelegateInfo {..} =
  O.DelegateInfo
    { O.di'GroupName =
        di'GroupName & toPlutusTextUtf8
    , --
      O.di'GroupURL =
        di'GroupURL & toPlutusTextUtf8
    , --
      O.di'Delegates =
        di'Delegates <&> toPlutusVKeyHash
    }
