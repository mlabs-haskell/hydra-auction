module HydraAuctionUtils.Fixture (
  Actor (..),
  keysFor,
  chainConfigFor,
  readProtocolParams,
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
  ProtocolParameters,
  fromLedgerPParams,
  ShelleyBasedEra (ShelleyBasedEraShelley)
 )

-- Haskell imports

import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import System.FilePath ((<.>), (</>))

-- Hydra Auction imports
import Paths_hydra_auction qualified as Pkg
import Hydra.Options (ChainConfig (..), defaultChainConfig)
import Control.Monad (when, forM_)
import Hydra.Cluster.Fixture (cperiod)

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

-- | Lookup a config file similar reading a file from disk.
readDataFile :: FilePath -> IO BS.ByteString
readDataFile source = do
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

data KeyKind = Verification | Secret

-- | Copy required keys to temp dir and create ChainConfig using them
chainConfigFor :: Actor -> FilePath -> FilePath -> [Actor] -> IO ChainConfig
chainConfigFor me targetDir nodeSocket them = do
  when (me `elem` them) $
    fail $ show me <> " must not be in " <> show them
  copyKeyToTargetDir Verification me
  copyKeyToTargetDir Secret me
  forM_ them $ \actor -> copyKeyToTargetDir Verification actor
  return $
    defaultChainConfig
      { nodeSocket
      , cardanoSigningKey = keyTargetPath Secret me
      , cardanoVerificationKeys =
         [keyTargetPath Verification actor | actor <- them]
      , contestationPeriod = cperiod
      }
 where
  keyTargetPath kind x = targetDir </> keyName kind x
  keyName kind x = actorName x <.> (keyExtension kind)
  keyExtension kind = case kind of
    Verification -> "vk"
    Secret -> "sk"
  copyKeyToTargetDir kind actor = do
    configBytes <- readDataFile ("credentials" </> keyName kind actor)
    BS.writeFile (keyTargetPath kind actor) configBytes

readProtocolParams :: IO ProtocolParameters
readProtocolParams = do
  bytes <- readDataFile "protocol-parameters.json"
  case Aeson.eitherDecodeStrict bytes of
    Left errorMsg -> fail $
      "Cannot decode protocol-parameters.json, error: " <> show errorMsg
    Right protocolParams -> return protocolParams
