module HydraAuction.Onchain.MintingPolicies.Auction (
  AuctionMP'Redeemer (..),
  mintingPolicy,
  unappliedMintingPolicy,
  mintingPolicyScript,
) where

import PlutusTx.Prelude

import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1.Scripts (
  Datum (..),
  ScriptHash,
 )
import PlutusLedgerApi.V1.Value (
  Value (..),
  valueOf,
 )
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (..),
  TxInfo (..),
  TxOutRef (..),
  findTxInByTxOutRef,
  ownCurrencySymbol,
 )
import PlutusLedgerApi.V2.Tx (
  OutputDatum (..),
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

import HydraAuction.Error.Onchain.MintingPolicies.Auction (
  AuctionBurn'Error (..),
  AuctionMint'Error (..),
 )
import HydraAuction.Onchain.Lib.Error (eCode, err, errMaybe)
import HydraAuction.Onchain.Lib.PlutusScript (
  MintingPolicyType,
  wrapMintingPolicy,
 )
import HydraAuction.Onchain.Lib.ScriptContext (scriptOutputsAt)
import HydraAuction.Onchain.Types.AuctionInfo (
  AuctionInfo (..),
  auctionMetadataTN,
  auctionTN,
  standingBidTN,
 )
import HydraAuction.Onchain.Types.AuctionTerms (validateAuctionTerms)

import HydraAuction.Onchain.MintingPolicies.AuctionRedeemer (
  AuctionMP'Redeemer (..),
 )
import HydraAuction.Onchain.Validators.AuctionMetadata qualified as AMetadata

-- -------------------------------------------------------------------------
-- Minting policy
-- -------------------------------------------------------------------------
{-# INLINEABLE mintingPolicy #-}
mintingPolicy ::
  ScriptHash ->
  TxOutRef ->
  AuctionMP'Redeemer ->
  ScriptContext ->
  Bool
mintingPolicy metadataValidator utxoNonce action context =
  case action of
    MintAuction ->
      checkMint metadataValidator utxoNonce context
    BurnAuction ->
      checkBurn context

{-# INLINEABLE checkMint #-}
checkMint ::
  ScriptHash ->
  TxOutRef ->
  ScriptContext ->
  Bool
checkMint metadataValidator utxoNonce context =
  utxoNonceIsSpent
    && auctionTokensAreMintedExactly
    && auctionMetadataOutputExistsAndMatchesOwnCS
    && auctionMetadataOutputContainsMetadataToken
    && auctionTermsValid
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    ownCS = ownCurrencySymbol context
    --
    -- (AuctionMint01)
    -- The utxo nonce parameter of the minting policy
    -- should be a reference to a utxo input spent
    -- by the transaction.
    utxoNonceIsSpent =
      isJust (findTxInByTxOutRef utxoNonce txInfo)
        `err` $(eCode AuctionMint'Error'MissingUtxoNonceInput)
    --
    -- (AuctionMint02)
    -- There should be an output with an auction info metadata record
    -- that mentions an auction ID that matches
    -- this minting policy's currency symbol.
    auctionMetadataOutputExistsAndMatchesOwnCS =
      (ownCS == ai'AuctionId)
        `err` $(eCode AuctionMint'Error'AuctionInfoMismatchedToken)
    --
    -- (AuctionMint03)
    -- The auction metadata output should contain
    -- an auction metadata token with a currency symbol
    -- matching this minting policy.
    auctionMetadataOutputContainsMetadataToken =
      (valueOf aiValue ownCS auctionMetadataTN == 1)
        `err` $(eCode AuctionMint'Error'MetadataOutputMissingToken)
    --
    -- (AuctionMint04)
    -- The auction metadata record should contain valid auction terms.
    auctionTermsValid =
      validateAuctionTerms ai'AuctionTerms
        `err` $(eCode $ AuctionMint'Error'InvalidAuctionTerms [])
    --
    -- (AuctionMint05)
    -- The auction state, auction metadata, and standing bid tokens
    -- of the auction should all be minted.
    -- No other tokens should be minted or burned.
    auctionTokensAreMintedExactly =
      (txInfoMint == expectedMint)
        `err` $(eCode AuctionMint'Error'AuctionTokensNotMinted)
    expectedMint =
      Value $
        AssocMap.singleton ownCS $
          AssocMap.fromList
            [ (auctionTN, 1)
            , (auctionMetadataTN, 1)
            , (standingBidTN, 1)
            ]
    --
    -- (AuctionMint06)
    -- The auction metadata output's datum should be decodable
    -- as an auction info metadata record.
    AuctionInfo {..} =
      PlutusTx.fromBuiltinData
        (getDatum aiDatum)
        `errMaybe` $(eCode AuctionMint'Error'FailedToDecodeMetadataDatum)
    (aiDatum, aiValue) =
      case scriptOutputsAt metadataValidator txInfo of
        --
        -- (AuctionMint07)
        -- The auction metadata output should contain an inline datum.
        [(od, v)]
          | OutputDatum d <- od ->
              (d, v)
          | otherwise ->
              traceError $(eCode AuctionMint'Error'MetadataOutputMissingDatum)
        [] ->
          --
          -- (AuctionMint08)
          -- There should be an auction metadata output.
          traceError $(eCode AuctionMint'Error'MissingMetadataOutput)
        _tooMany ->
          --
          -- (AuctionMint09)
          -- There should only be one auction metadata output.
          traceError $(eCode AuctionMint'Error'TooManyMetadataOutputs)

{-# INLINEABLE checkBurn #-}
checkBurn ::
  ScriptContext ->
  Bool
checkBurn context =
  auctionTokensAreBurnedExactly
  where
    TxInfo {..} = scriptContextTxInfo context
    ownCS = ownCurrencySymbol context
    --
    -- (AuctionBurn01)
    -- The auction state, auction metadata, and standing bid tokens
    -- of the auction should all be burned.
    -- No other tokens should be minted or burned.
    auctionTokensAreBurnedExactly =
      (txInfoMint == expectedMint)
        `err` $(eCode AuctionBurn'Error'AuctionTokensNotBurned)
    expectedMint =
      Value $
        AssocMap.singleton ownCS $
          AssocMap.fromList
            [ (auctionTN, -1)
            , (auctionMetadataTN, -1)
            , (standingBidTN, -1)
            ]

-- -------------------------------------------------------------------------
-- Script compilation
-- -------------------------------------------------------------------------

-- | Raw minting policy code where the 'TxOutRef' is still a parameter.
unappliedMintingPolicy ::
  PlutusTx.CompiledCode (TxOutRef -> MintingPolicyType)
unappliedMintingPolicy =
  $$(PlutusTx.compile [||\vMetadata ref -> wrapMintingPolicy (mintingPolicy vMetadata ref)||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 AMetadata.validatorHash

-- | Get the applied head minting policy script given a seed 'TxOutRef'.
mintingPolicyScript :: TxOutRef -> SerialisedScript
mintingPolicyScript txOutRef =
  serialiseCompiledCode $
    unappliedMintingPolicy
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 txOutRef
