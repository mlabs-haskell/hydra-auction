module HydraAuction.Onchain.Types.DelegateInfo (
  DelegateInfo (..),
  DelegateInfo'Error (..),
  validateDelegateInfo,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V2 (PubKeyHash)
import PlutusTx qualified

import HydraAuction.Error.Types.DelegateInfo (DelegateInfo'Error (..))
import HydraAuction.Onchain.Lib.Error (eCode, err)

data DelegateInfo = DelegateInfo
  { di'GroupName :: BuiltinByteString
  , di'GroupUrl :: BuiltinByteString
  , di'Delegates :: [PubKeyHash]
  -- ^ The payment vkey hashes of the individual delegates in this group.
  -- These payment vkey hashes also indicate where the delegates
  -- should receive payment for their services.
  }

{- FOURMOLU_DISABLE -}
instance Eq DelegateInfo where
  ( DelegateInfo x1 x2 x3) ==
   (DelegateInfo y1 y2 y3) =
    x1 == y1 &&
    x2 == y2 &&
    x3 == y3
{- FOURMOLU_ENABLE -}

PlutusTx.unstableMakeIsData ''DelegateInfo

-- -------------------------------------------------------------------------
-- Validation
-- -------------------------------------------------------------------------

validateDelegateInfo ::
  DelegateInfo ->
  Bool
validateDelegateInfo DelegateInfo {..} =
  --
  -- There must be at least one delegate.
  (length di'Delegates > 0)
    `err` $(eCode DelegateInfo'Error'NoDelegates)
--
{-# INLINEABLE validateDelegateInfo #-}
