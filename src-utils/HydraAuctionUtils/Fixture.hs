module HydraAuctionUtils.Fixture (
  Actor (..),
  keysFor,
  hydraNodeActors,
  partyFor,
  getActorPubKeyHash,
  getActorsPubKeyHash,
) where

-- Prelude imports
import Prelude

-- Haskell imports
import Control.Monad (unless)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import GHC.Generics (Generic)
import System.FilePath ((<.>), (</>))

-- Hydra imports
import Hydra.Cardano.Api (
  AsType (AsPaymentKey, AsSigningKey, AsVerificationKey),
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

-- Hydra Auction imports
import Hydra.Crypto (AsType (AsHydraKey))
import Hydra.Party (Party (Party))
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
  | Rupert
  | Oscar
  | Patricia
  deriving stock (Eq, Show, Enum, Bounded, Generic)

instance Aeson.FromJSON Actor
instance Aeson.ToJSON Actor

-- These actors are supposed to be used as Hydra node admins
hydraNodeActors :: [Actor]
hydraNodeActors = [Rupert, Oscar, Patricia]

-- | Get the "well-known" keys for given actor.
keysFor :: Actor -> IO (VerificationKey PaymentKey, SigningKey PaymentKey)
keysFor actor = do
  bs <- readDataFile $ "credentials" </> actorName actor <.> "sk"
  let res =
        first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict bs)
          >>= deserialiseFromTextEnvelope asSigningKey
  case res of
    Left err ->
      fail $
        "cannot decode text envelope from '"
          <> show bs
          <> "', error: "
          <> show err
    Right sk -> pure (getVerificationKey sk, sk)
  where
    asSigningKey :: AsType (SigningKey PaymentKey)
    asSigningKey = AsSigningKey AsPaymentKey

partyFor :: Actor -> IO Party
partyFor actor = do
  unless (actor `notElem` hydraNodeActors) $
    fail "Only Hydra Node actors do have hydra keys and party"
  bs <-
    readDataFile $
      "hydra-keys" </> actorName actor <.> "vk"
  let res =
        first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict bs)
          >>= deserialiseFromTextEnvelope (AsVerificationKey AsHydraKey)
  case res of
    Left err ->
      fail $ "cannot decode text envelope from '" <> show bs <> "', error: " <> show err
    Right vk -> return $ Party vk

getActorPubKeyHash :: Actor -> IO PubKeyHash
getActorPubKeyHash actor = do
  (actorVk, _) <- keysFor actor
  return $ toPlutusKeyHash $ verificationKeyHash actorVk

getActorsPubKeyHash :: [Actor] -> IO [PubKeyHash]
getActorsPubKeyHash actors = sequence $ getActorPubKeyHash <$> actors

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
  Rupert -> "rupert"
  Oscar -> "oscar"
  Patricia -> "patricia"
