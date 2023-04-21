module CLI.Types (CLIError (..), CLILog (..)) where

import Prettyprinter (Doc, Pretty (pretty), indent, line)
import Prelude

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
