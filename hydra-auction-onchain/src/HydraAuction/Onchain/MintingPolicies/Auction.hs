module HydraAuction.Onchain.MintingPolicies.Auction (
  AuctionMP'Redeemer (..),
  mintingPolicy,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V2.Contexts (
  ScriptContext (..),
  TxInfo (..),
  TxOutRef (..),
  findTxInByTxOutRef,
  ownCurrencySymbol,
 )

import HydraAuction.Error.Onchain.MintingPolicies.Auction (
  AuctionMP'Error (..),
 )
import HydraAuction.Onchain.Lib.Error (eCode, err, errMaybe)
import HydraAuction.Onchain.Lib.PlutusTx (parseInlineDatum)
import HydraAuction.Onchain.Types.AuctionInfo (
  AuctionInfo (..),
 )
import HydraAuction.Onchain.Types.AuctionTerms (validateAuctionTerms)
import HydraAuction.Onchain.Types.Scripts (
  AuctionID (..),
  AuctionMP'Redeemer (..),
  AuctionMetadata'ScriptHash (..),
  allAuctionTokensBurned,
  allAuctionTokensMinted,
  findAuctionMetadataTxOutAtSh,
 )

-- -------------------------------------------------------------------------
-- Minting policy
-- -------------------------------------------------------------------------
mintingPolicy ::
  AuctionMetadata'ScriptHash ->
  TxOutRef ->
  AuctionMP'Redeemer ->
  ScriptContext ->
  Bool
mintingPolicy v utxoNonce action context =
  case action of
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
    && auctionMetadataOutputExistsAndMatchesOwnCS
    && auctionTermsValid
    && auctionTokensAreMintedExactly
  where
    txInfo@TxInfo {..} = scriptContextTxInfo context
    ownCS = ownCurrencySymbol context
    auctionID = AuctionID ownCS
    --
    -- The utxo nonce parameter of the minting policy
    -- should be a reference to a utxo input spent
    -- by the transaction.
    utxoNonceIsSpent =
      isJust (findTxInByTxOutRef utxoNonce txInfo)
        `err` $(eCode AuctionMP'Error'MI'MissingUtxoNonceInput)
    --
    -- There should be an output with an auction info metadata record
    -- that mentions an auction ID that matches
    -- this minting policy's currency symbol.
    auctionMetadataOutputExistsAndMatchesOwnCS =
      (ownCS == ai'AuctionId)
        `err` $(eCode AuctionMP'Error'MI'AuctionInfoMismatchedToken)
    --
    -- The auction metadata record should contain valid auction terms.
    auctionTermsValid =
      validateAuctionTerms ai'AuctionTerms
        `err` $(eCode $ AuctionMP'Error'MI'InvalidAuctionTerms [])
    --
    -- The auction state, auction metadata, and standing bid tokens
    -- of the auction should all be minted.
    -- No other tokens should be minted or burned.
    auctionTokensAreMintedExactly =
      (txInfoMint == allAuctionTokensMinted auctionID)
        `err` $(eCode AuctionMP'Error'MI'AuctionTokensNotMinted)
    --
    -- The auction metadata output contains a datum that can be
    -- decoded as an auction info metadata record.
    AuctionInfo {..} =
      parseInlineDatum auctionMetadataOutput
        `errMaybe` $(eCode AuctionMP'Error'MI'FailedToDecodeMetadataDatum)
    --
    -- There is an output at the auction metadata validator
    -- containing the auction metadata token.
    auctionMetadataOutput =
      findAuctionMetadataTxOutAtSh auctionID auctionMetadataSh txInfoOutputs
        `errMaybe` $(eCode AuctionMP'Error'MI'MissingMetadataOutput)
--
{-# INLINEABLE checkMint #-}

checkBurn ::
  ScriptContext ->
  Bool
checkBurn context =
  auctionTokensAreBurnedExactly
  where
    TxInfo {..} = scriptContextTxInfo context
    ownCS = ownCurrencySymbol context
    auctionID = AuctionID ownCS
    --
    -- The auction state, auction metadata, and standing bid tokens
    -- of the auction should all be burned.
    -- No other tokens should be minted or burned.
    auctionTokensAreBurnedExactly =
      (txInfoMint == allAuctionTokensBurned auctionID)
        `err` $(eCode AuctionMP'Error'BU'AuctionTokensNotBurned)
--
{-# INLINEABLE checkBurn #-}
