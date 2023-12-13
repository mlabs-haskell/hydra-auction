module HydraAuction.Offchain.Types.DelegateInfo (
  DelegateInfo (..),
  DelegateInfo'Error (..),
  validateDelegateInfo,
  toPlutusDelegateInfo,
  fromPlutusDelegateInfo,
) where

import GHC.Generics (Generic)
import Prelude

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Traversable (for)
import Data.Validation (Validation)

import HydraAuction.Error.Types.DelegateInfo (
  DelegateInfo'Error (..),
 )
import HydraAuction.Offchain.Lib.Crypto (
  Hash,
  PaymentKey,
 )
import HydraAuction.Offchain.Lib.Validation (err)
import Plutus.Cardano.Api.Codec (
  fromPlutusTextUtf8,
  fromPlutusVKeyHash,
  toPlutusTextUtf8,
  toPlutusVKeyHash,
 )

import HydraAuction.Onchain.Types.DelegateInfo qualified as O

data DelegateInfo = DelegateInfo
  { di'GroupName :: !Text
  , di'GroupUrl :: !Text
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
  -- There must be at least one delegate.
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
      O.di'GroupUrl =
        di'GroupUrl & toPlutusTextUtf8
    , --
      O.di'Delegates =
        di'Delegates <&> toPlutusVKeyHash
    }

-- -------------------------------------------------------------------------
-- Conversion from onchain
-- -------------------------------------------------------------------------
fromPlutusDelegateInfo :: O.DelegateInfo -> Maybe DelegateInfo
fromPlutusDelegateInfo O.DelegateInfo {..} = do
  m'di'GroupName <-
    di'GroupName & fromPlutusTextUtf8
  --
  m'di'GroupUrl <-
    di'GroupUrl & fromPlutusTextUtf8
  --
  m'di'Delegates <-
    di'Delegates `for` fromPlutusVKeyHash
  --
  pure $
    DelegateInfo
      { di'GroupName = m'di'GroupName
      , di'GroupUrl = m'di'GroupUrl
      , di'Delegates = m'di'Delegates
      }
