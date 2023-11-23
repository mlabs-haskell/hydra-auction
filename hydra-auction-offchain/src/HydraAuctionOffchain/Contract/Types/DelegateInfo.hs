module HydraAuctionOffchain.Contract.Types.DelegateInfo (
  DelegateInfo (..),
  validateDelegateInfo,
) where

import Prelude

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Validation
import GHC.Generics (Generic)

import HydraAuctionOffchain.Lib.Crypto (
  Hash,
  PaymentKey,
 )

data DelegateInfo = DelegateInfo
  { di'GroupName :: Text
  , di'GroupURL :: Text
  , di'Delegates :: [Hash PaymentKey]
  }
  deriving stock (Eq, Generic, Show)

data DelegateInfoValidationError
  = NoDelegatesInDelegateInfoError

validateDelegateInfo ::
  DelegateInfo ->
  Validation [DelegateInfoValidationError] ()
validateDelegateInfo DelegateInfo {..} =
  fold
    [ (length di'Delegates > 0)
        `err` NoDelegatesInDelegateInfoError
    ]
  where
    err x e = if x then Success () else Failure [e]
