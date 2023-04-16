module CLI.Types (CLIError (..), UserError (..), CLILog (..), unrecoverable) where

import HydraAuction.Delegate.Interface (DelegateResponse)
import Prettyprinter (Doc, Pretty (pretty), indent, line, (<+>))
import Prelude

newtype CLILog
  = CLIError CLIError

data CLIError
  = UserError UserError
  | UnimplementedResponse DelegateResponse
  | InvalidResponse String

newtype UserError
  = EnvVarMissing String

instance Pretty CLIError where
  pretty = \case
    UserError usererror -> "User error occured:" <> extraInfo (pretty usererror)
    UnimplementedResponse response ->
      "Cannot handle delegate response:"
        <> extraInfo (pretty response)
    InvalidResponse str ->
      "Delegate gave an invalid response:"
        <> extraInfo (pretty str)

instance Pretty UserError where
  pretty = \case
    EnvVarMissing varName -> "Failed to look up" <+> pretty varName <+> "env var"

unrecoverable :: MonadFail m => CLIError -> m void
unrecoverable = fail . show . pretty

-- | additional information on a log event
extraInfo :: forall ann. Doc ann -> Doc ann
extraInfo = (line <>) . indent 2
