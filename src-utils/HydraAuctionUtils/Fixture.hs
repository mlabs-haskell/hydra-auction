module HydraAuctionUtils.Fixture (
  Actor (..),
  ActorKind (..),
  actorName,
  keysFor,
  hydraKeysFor,
  actorsByKind,
  partyFor,
  allActors,
  actorFromPkh,
  actorFromSk,
  getActorPubKeyHash,
  getActorsPubKeyHash,
  getActorKind,
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Haskell imports
import Control.Monad.Extra (findM, unless)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.List qualified as List
import Data.Map qualified as Map
import System.FilePath ((<.>), (</>))

-- Hydra imports
import Hydra.Cardano.Api (
  AsType (AsPaymentKey, AsSigningKey),
  HasTypeProxy (AsType),
  Key (VerificationKey, getVerificationKey),
  PaymentKey (..),
  SigningKey (..),
  TextEnvelopeError (TextEnvelopeAesonDecodeError),
  deserialiseFromTextEnvelope,
  toPlutusKeyHash,
  verificationKeyHash,
 )

-- Plutus imports
import PlutusLedgerApi.V1.Crypto (PubKeyHash)

-- Hydra Auction imports
import Hydra.Crypto (AsType (AsHydraKey), HydraKey)
import Hydra.Party (Party, deriveParty)
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
  | Oscar
  | Patricia
  | Rupert
  | Faucet
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

instance Aeson.FromJSON Actor
instance Aeson.ToJSON Actor

allActors :: [Actor]
allActors = [minBound .. maxBound]

-- HydraNodeActor are supposed to be used as Hydra node admins
data ActorKind = RegularActor | HydraNodeActor | FaucetActor
  deriving stock (Show, Eq, Ord, Enum, Bounded)

actorsByKind :: Map ActorKind [Actor]
actorsByKind =
  Map.fromList
    [ (RegularActor, [minBound .. Hans])
    , (HydraNodeActor, [Oscar, Patricia, Rupert])
    , (FaucetActor, [Faucet])
    ]

getActorKind :: Actor -> ActorKind
getActorKind actor
  | actor `elem` (Map.!) actorsByKind RegularActor =
      RegularActor
  | actor `elem` (Map.!) actorsByKind HydraNodeActor =
      HydraNodeActor
  | otherwise = FaucetActor

{- | Get the "well-known" keys for given actor.
 FIXME: cache this
-}
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

deriving instance Eq (SigningKey PaymentKey)

actorFromSk :: SigningKey PaymentKey -> IO (Maybe Actor)
actorFromSk key = do
  allKeys <- mapM keysFor allActors
  let keyToActor = zip (map snd allKeys) allActors
  -- No `Ord (SigningKey PaymentKey)` available
  return $ snd <$> List.find ((key ==) . fst) keyToActor

partyFor :: Actor -> IO Party
partyFor actor = do
  (_, sk) <- hydraKeysFor actor
  pure $ deriveParty sk

hydraKeysFor :: Actor -> IO (VerificationKey HydraKey, SigningKey HydraKey)
hydraKeysFor actor = do
  unless (getActorKind actor == HydraNodeActor) $
    fail "Only Hydra Node actors do have hydra keys"
  bs <-
    readDataFile $
      "hydra-keys" </> actorName actor <.> "sk"
  let res =
        first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict bs)
          >>= deserialiseFromTextEnvelope (AsSigningKey AsHydraKey)
  case res of
    Left err ->
      fail $ "cannot decode text envelope from '" <> show bs <> "', error: " <> show err
    Right sk -> return (getVerificationKey sk, sk)

getActorPubKeyHash :: Actor -> IO PubKeyHash
getActorPubKeyHash actor = do
  (actorVk, _) <- keysFor actor
  return $ toPlutusKeyHash $ verificationKeyHash actorVk

getActorsPubKeyHash :: [Actor] -> IO [PubKeyHash]
getActorsPubKeyHash = mapM getActorPubKeyHash

actorFromPkh :: PubKeyHash -> IO Actor
actorFromPkh pkh = do
  mbActor <-
    findM
      ( \actor -> do
          actorPkh <- getActorPubKeyHash actor
          pure $ actorPkh == pkh
      )
      allActors
  case mbActor of
    Just actor -> pure actor
    Nothing -> fail $ "Unable to find actor matching key: " <> show pkh

actorName :: Actor -> String
actorName = map toLower . show
