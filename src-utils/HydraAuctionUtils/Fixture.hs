module HydraAuctionUtils.Fixture (
  Actor (..),
  keysFor,
  getActorsPubKey,
  getActorVkHash,
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
  toPlutusKeyHash,
  verificationKeyHash,
 )

-- Plutus imports
import Plutus.V1.Ledger.Crypto (PubKeyHash)

-- Haskell imports
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import GHC.Generics (Generic)
import System.FilePath ((<.>), (</>))

-- Hydra Auction imports
import HydraAuctionUtils.BundledData (readDataFile)

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
  deriving stock (Eq, Show, Enum, Bounded, Generic)

instance Aeson.FromJSON Actor
instance Aeson.ToJSON Actor

-- | Get the "well-known" keys for given actor.
keysFor :: Actor -> IO (VerificationKey PaymentKey, SigningKey PaymentKey)
keysFor actor = do
  bs <- readDataFile ("credentials" </> actorName actor <.> "sk")
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


getActorVkHash :: Actor -> IO PubKeyHash
getActorVkHash actor = do
  (actorVk, _) <- keysFor actor
  return $ toPlutusKeyHash $ verificationKeyHash actorVk

getActorsPubKey :: [Actor] -> IO [PubKeyHash]
getActorsPubKey actors = sequence $ getActorVkHash <$> actors

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
