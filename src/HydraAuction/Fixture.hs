module HydraAuction.Fixture (
  Actor (..),
  keysFor,
) where

-- Prelude imports
import Prelude

-- Hydra imports
import Hydra.Cardano.Api (
  AsType (AsPaymentKey, AsSigningKey),
  HasTypeProxy (AsType),
  Key (VerificationKey, getVerificationKey),
  PaymentKey,
  SigningKey,
  TextEnvelopeError (TextEnvelopeAesonDecodeError),
  deserialiseFromTextEnvelope,
 )

-- Haskell imports

import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import Paths_hydra_auction qualified as Pkg
import System.FilePath ((<.>), (</>))

-- | Enumeration of known actors for which we can get the 'keysFor' and 'writeKeysFor'.
data Actor
  = Alice
  | Bob
  | Carol
  | Dave
  | Eve
  | Frank
  | Grace
  | Hans
  | Faucet
  deriving stock (Eq, Show, Enum, Bounded, Generic)

instance Aeson.FromJSON Actor
instance Aeson.ToJSON Actor

-- | Get the "well-known" keys for given actor.
keysFor :: Actor -> IO (VerificationKey PaymentKey, SigningKey PaymentKey)
keysFor actor = do
  bs <- readConfigFile ("credentials" </> actorName actor <.> "sk")
  let res =
        first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict bs)
          >>= deserialiseFromTextEnvelope asSigningKey
  case res of
    Left err ->
      fail $ "cannot decode text envelope from '" <> show bs <> "', error: " <> show err
    Right sk -> pure (getVerificationKey sk, sk)
  where
    asSigningKey :: AsType (SigningKey PaymentKey)
    asSigningKey = AsSigningKey AsPaymentKey

-- | Lookup a config file similar reading a file from disk.
readConfigFile :: FilePath -> IO BS.ByteString
readConfigFile source = do
  filename <- Pkg.getDataFileName ("data" </> source)
  BS.readFile filename

actorName :: Actor -> String
actorName = \case
  Alice -> "alice"
  Bob -> "bob"
  Carol -> "carol"
  Dave -> "dave"
  Eve -> "eve"
  Frank -> "frank"
  Grace -> "grace"
  Hans -> "hans"
  Faucet -> "faucet"
