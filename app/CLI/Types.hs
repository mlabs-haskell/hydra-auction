module CLI.Types (CLIError (..), CLILog (..)) where

import HydraAuction.Delegate.Interface (DelegateResponse)
import Prettyprinter (Doc, Pretty (pretty), indent, line)
import Prelude

newtype CLILog
  = CLIError CLIError

data CLIError
  = NotImplementedDelegateResponse DelegateResponse
  | InvalidDelegateResponse String

instance Pretty CLIError where
  pretty = \case
    NotImplementedDelegateResponse response ->
      "Cannot handle delegate response:"
        <> extraInfo (pretty response)
    InvalidDelegateResponse str ->
      "Delegate gave an invalid response:"
        <> extraInfo (pretty str)

-- | additional information on a log event
extraInfo :: forall ann. Doc ann -> Doc ann
extraInfo = (line <>) . indent 2
