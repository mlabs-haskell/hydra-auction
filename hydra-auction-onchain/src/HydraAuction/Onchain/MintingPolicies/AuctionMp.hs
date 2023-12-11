module HydraAuction.Onchain.MintingPolicies.AuctionMp (
  AuctionMp'Redeemer (..),
  mintingPolicy,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V2 (
  ScriptContext (..),
  TxInfo (..),
  TxOutRef (..),
 )
import PlutusLedgerApi.V2.Contexts (
  findTxInByTxOutRef,
  ownCurrencySymbol,
 )

import HydraAuction.Error.Onchain.MintingPolicies.AuctionMp (
  AuctionMp'Error (..),
 )
import HydraAuction.Onchain.Lib.Error (eCode, err, errMaybe)
import HydraAuction.Onchain.Lib.PlutusTx (parseInlineDatum)
import HydraAuction.Onchain.Types.AuctionInfo (
  AuctionInfo (..),
 )
import HydraAuction.Onchain.Types.AuctionTerms (validateAuctionTerms)
import HydraAuction.Onchain.Types.Redeemers (
  AuctionMp'Redeemer (..),
 )
import HydraAuction.Onchain.Types.Scripts (
  AuctionMetadata'ScriptHash (..),
  findAuctionMetadataTxOutAtSh,
 )
import HydraAuction.Onchain.Types.Tokens (
  AuctionId (..),
  allAuctionTokensBurned,
  allAuctionTokensMinted,
 )

-- -------------------------------------------------------------------------
-- Minting policy
-- -------------------------------------------------------------------------
mintingPolicy ::
  AuctionMetadata'ScriptHash ->
  TxOutRef ->
  AuctionMp'Redeemer ->
  ScriptContext ->
  Bool
mintingPolicy v utxoNonce redeemer context =
  case redeemer of
    MintAuction ->
      checkMint v utxoNonce context
    BurnAuction ->
      checkBurn context
--
{-# INLINEABLE mintingPolicy #-}

checkMint ::
  AuctionMetadata'ScriptHash ->
  TxOutRef ->
  ScriptContext ->
  Bool
checkMint auctionMetadataSh utxoNonce context =
  utxoNonceIsSpent
    && auctionMetadataOutputExistsAndMatchesOwnCs
    && auctionTermsValid
    && auctionTokensAreMintedExactly
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    ownCs = ownCurrencySymbol context
    auctionId = AuctionId ownCs
    --
    -- The utxo nonce parameter of the minting policy
    -- should be a reference to a utxo input spent
    -- by the transaction.
    utxoNonceIsSpent =
      isJust (findTxInByTxOutRef utxoNonce txInfo)
        `err` $(eCode AuctionMp'MI'Error'MissingUtxoNonceInput)
    --
    -- There should be an output with an auction info metadata record
    -- that mentions an auction ID that matches
    -- this minting policy's currency symbol.
    auctionMetadataOutputExistsAndMatchesOwnCs =
      (ownCs == ai'AuctionId)
        `err` $(eCode AuctionMp'MI'Error'AuctionInfoMismatchedToken)
    --
    -- The auction metadata record should contain valid auction terms.
    auctionTermsValid =
      validateAuctionTerms ai'AuctionTerms
        `err` $(eCode $ AuctionMp'MI'Error'InvalidAuctionTerms [])
    --
    -- The auction state, auction metadata, and standing bid tokens
    -- of the auction should all be minted.
    -- No other tokens should be minted or burned.
    auctionTokensAreMintedExactly =
      (txInfoMint == allAuctionTokensMinted auctionId)
        `err` $(eCode AuctionMp'MI'Error'AuctionTokensNotMinted)
    --
    -- The auction metadata output contains a datum that can be
    -- decoded as an auction info metadata record.
    AuctionInfo {..} =
      parseInlineDatum auctionMetadataOutput
        `errMaybe` $(eCode AuctionMp'MI'Error'FailedToDecodeMetadataDatum)
    --
    -- There is an output at the auction metadata validator
    -- containing the auction metadata token.
    auctionMetadataOutput =
      findAuctionMetadataTxOutAtSh auctionId auctionMetadataSh txInfoOutputs
        `errMaybe` $(eCode AuctionMp'MI'Error'MissingMetadataOutput)
--
{-# INLINEABLE checkMint #-}

checkBurn ::
  ScriptContext ->
  Bool
checkBurn context =
  auctionTokensAreBurnedExactly
  where
    TxInfo {..} = scriptContextTxInfo context
    ownCs = ownCurrencySymbol context
    auctionId = AuctionId ownCs
    --
    -- The auction state, auction metadata, and standing bid tokens
    -- of the auction should all be burned.
    -- No other tokens should be minted or burned.
    auctionTokensAreBurnedExactly =
      (txInfoMint == allAuctionTokensBurned auctionId)
        `err` $(eCode AuctionMp'BU'Error'AuctionTokensNotBurned)
--
{-# INLINEABLE checkBurn #-}
