{-# OPTIONS -Wno-orphans #-}

-- | Pure logic and Hydra communication used for Delegate server
module HydraAuction.Delegate (
  delegateFrontendRequestStep,
  delegateEventStep,
  DelegateEvent (..),
) where

-- Prelude imports
import HydraAuctionUtils.Prelude

-- Hydra imports

import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (
  CtxUTxO,
  Lovelace (..),
  ShelleyWitnessSigningKey (WitnessPaymentKey),
  TxOut,
  getTxBody,
  getTxWitnesses,
  makeShelleyKeyWitness,
  makeSignedTransaction,
  toPlutusKeyHash,
  -- txIns,
  verificationKeyHash,
 )
import Hydra.Chain (HeadId)
import Hydra.Chain.Direct.Tx (headIdToCurrencySymbol)

-- HydraAuction imports

import HydraAuction.Delegate.Interface (
  CommitAction (..),
  CustomEvent (..),
  DelegateProtocol,
  OpenHeadUtxo (..),
  OpenState (..),
  TxAction (..),
 )
import HydraAuction.OnChain.Common (validAuctionTerms)
import HydraAuction.OnChain.StandingBid (validNewBidTerms)
import HydraAuction.Platform.Interface (
  AnnouncedAuction (..),
  ClientCommand (..),
  ClientInput (..),
  EntityFilter (..),
  EntityKind (..),
  EntityQuery (..),
  EntityQueryResponse (..),
  FilterEq (..),
  PlatformProtocol,
  ServerOutput (..),
  Some (..),
 )
import HydraAuction.Tx.FeeEscrow (
  distributeFee,
 )
import HydraAuction.Tx.StandingBid (
  NewBidTxInfo (..),
  createNewBidTx,
  decodeInlineDatum,
  decodeNewBidTxOnL2,
  moveToHydraTx,
 )
import HydraAuction.Types (
  AuctionStage (..),
  AuctionTerms (..),
  BidTerms (..),
  StandingBidDatum (..),
  StandingBidState (..),
 )
import HydraAuctionUtils.Delegate.Interface
import HydraAuctionUtils.Delegate.Logic (
  DelegateLogic (..),
  SingleClientScope (..),
  delegateEventStep,
  delegateFrontendRequestStep,
 )
import HydraAuctionUtils.Fixture (Actor (..))
import HydraAuctionUtils.Hydra.Monad (MonadHydra (..))
import HydraAuctionUtils.Hydra.Runner (HydraRunner)
import HydraAuctionUtils.Monads (
  MonadSubmitTx (..),
  addressAndKeysForActor,
  submitAndAwaitTx,
 )
import HydraAuctionUtils.Monads.Actors (
  MonadHasActor (askActor),
  addressAndKeys,
 )
-- import HydraAuctionUtils.Tx.AutoCreateTx (makeSignedTransactionWithKeys)
import HydraAuctionUtils.Types.Natural (intToNatural)
import HydraAuctionUtils.WebSockets.Protocol (
  MonadHasClient (..),
  ProtocolClient (..),
  ProtocolClientFor,
  WithClientT,
 )

-- Common domain utils

validatingAuctionTerms ::
  Monad m =>
  HeadId ->
  AuctionTerms ->
  m [(SingleClientScope, DelegateResponse protocol)] ->
  m [(SingleClientScope, DelegateResponse protocol)]
validatingAuctionTerms headId auctionTerms cont =
  -- FIXME: On M6: validate delegates are matching actual Head Peers
  let termsHaveSameHeadId =
        hydraHeadId auctionTerms == headIdToCurrencySymbol headId
   in if not $
        validAuctionTerms auctionTerms
          && termsHaveSameHeadId
        then
          return
            [
              ( SameClient
              , RequestIgnored $
                  IncorrectRequestData
                    AuctionTermsAreInvalidOrNotMatchingHead
              )
            ]
        else cont

decodeStandingBidTerms ::
  TxOut CtxUTxO -> Maybe (Maybe BidTerms)
decodeStandingBidTerms txOut = case decodeInlineDatum txOut of
  Right standingBidDatum ->
    let StandingBidDatum {standingBidState} = standingBidDatum
        StandingBidState {standingBid} = standingBidState
     in Just standingBid
  Left _ -> Nothing

-- Instance

instance DelegateLogic DelegateProtocol where
  type DelegatePlatformProtocol DelegateProtocol = PlatformProtocol

  performCommitAction headId (MoveStandingBidToL2 {commitAuctionTerms, utxoToCommit}) =
    validatingAuctionTerms headId commitAuctionTerms $ do
      let (txIn, txOut) = utxoToCommit
      tx' <- moveToHydraTx headId commitAuctionTerms (txIn, txOut)
      (_, _, sk1) <- addressAndKeysForActor Faucet
      (_, _, sk2) <- addressAndKeys
      let
        body' = getTxBody tx'
        hydraSignWitness = getTxWitnesses tx'
        createWitness key =
          makeShelleyKeyWitness body' (WitnessPaymentKey key)
        keyWitnesses = fmap createWitness [sk1, sk2]
        tx = makeSignedTransaction (hydraSignWitness <> keyWitnesses) body'
      _ <- runL1RunnerInComposite $ submitAndAwaitTx tx

      return
        [
          ( BroadcastEveryone
          , CustomEventHappened $ AuctionSet commitAuctionTerms
          )
        ]

  performTxAction
    headId
    (NewBid {txAuctionTerms, datum})
    (MkOpenState utxoState _) = do
      let (MkOpenHeadUtxo {standingBidTerms, standingBidUtxo, collateralUtxo}) =
            utxoState
      validatingAuctionTerms headId txAuctionTerms $ do
        let newBidTerms = standingBid $ standingBidState datum
            newVoucherCS = standingBidVoucherCS datum
        if validNewBidTerms
          txAuctionTerms
          newVoucherCS
          standingBidTerms
          newBidTerms
          then do
            _ <- do
              let actor = Faucet
              tx <-
                createNewBidTx
                  txAuctionTerms
                  actor
                  standingBidUtxo
                  collateralUtxo
                  datum
              submitTx tx
            return []
          else
            return
              [
                ( SameClient
                , RequestIgnored $ IncorrectRequestData InvalidBidTerms
                )
              ]

  reactToCustomEvent state customEvent = case customEvent of
    AuctionStageStarted _ BiddingEndedStage -> do
      case state of
        NotInitialized -> return $ Left NobodyCommitedInTime
        Initialized {} -> do
          sendCommand Close
          return $ Right ()
    AuctionStageStarted terms CleanupStage -> do
      -- FIXME: handle case of not used Escrow Hydra
      result <- trySome $ runL1RunnerInComposite $ distributeFee terms
      case result of
        Left _ ->
          liftIO $
            putStrLn "Cannot distribute fee, probably no bids placed"
        Right _ -> return ()
      return $ Right ()
    AuctionStageStarted {} -> return $ Right ()
    AuctionSet _ -> return $ Right ()

  delegateEventHook ::
    forall client.
    (ProtocolClientFor PlatformProtocol client) =>
    DelegateEvent DelegateProtocol ->
    WithClientT
      client
      (StateT (DelegateState DelegateProtocol) HydraRunner)
      ()
  delegateEventHook event = do
    case event of
      HydraEvent (HeadIsInitializing {headId, parties}) -> do
        reportDelegateToPlatformServer headId parties
      _ -> return ()
    cacheAuctionTermsFromPlatformServer
    where
      reportDelegateToPlatformServer headId parties = do
        platformClient <- ask @client
        actor <- askActor
        let info =
              MkHydraHeadInfo
                { headId
                , delegatesNumber =
                    fromJust $ intToNatural $ toInteger $ length parties
                , auctionFeePerDelegate = Lovelace 4_000_000
                }
        sendInputH platformClient $
          MkSome HydraHead (Command $ ReportHeadDelegate info actor)
      cacheAuctionTermsFromPlatformServer = do
        state <- get
        case state of
          Initialized x (Open (MkOpenState value Nothing)) -> do
            mTerms <- queryAuctionTerms (error "FIXME")
            put $ Initialized x (Open (MkOpenState value mTerms))
          _ -> return ()
      queryAuctionTerms standingBid = do
        client <- askClient @client
        sendInputH client $
          MkSome AnnouncedAuction $
            Query $
              MkQuery
                { filters = [ByStandingBidAddress $ Eq standingBid]
                , limit = Nothing
                }
        response <- receiveOutputH client
        let result = case response of
              Just
                ( MkSome
                    AnnouncedAuction
                    (QueryPerformed (MkResponse [auction]))
                  ) ->
                  let MkAnnouncedAuction {auctionTerms} = auction
                   in Just auctionTerms
              _ -> Nothing
        return (result :: Maybe AuctionTerms)

  openStateUpdatedHook _ = return ()

  isCorrectCommit txOut = case decodeInlineDatum txOut of
    Right (_ :: StandingBidDatum) -> True
    Left _ -> False

  parseOpenStateFromUtxo (txIn, txOut) collateralUtxo = do
    standingBidTerms <- decodeStandingBidTerms txOut
    let utxoState =
          MkOpenHeadUtxo
            { standingBidTerms = standingBidTerms
            , standingBidUtxo = (txIn, txOut)
            , collateralUtxo = collateralUtxo
            }
    -- FIXME: cleaning cache to Nothing here, this is wrong
    return $ MkOpenState utxoState Nothing

  reactToTxInvalid
    transaction
    utxo
    (MkOpenState utxoState mCachedTerms) = do
      (_, delegatePublicKey, _) <- runL1RunnerInComposite addressAndKeys
      case (txResentRequired delegatePublicKey, mCachedTerms) of
        (Just newBidDatum, Just auctionTerms) -> do
          actor <- askActor
          _ <- resentBid actor auctionTerms newBidDatum
          return $ Right ()
        (Just _, Nothing) ->
          -- If tx was sent initialy by this server,
          -- auction terms should be already cached
          return $ Left $ ImpossibleHappened IncorrectHydraEvent
        (Nothing, _) -> return $ Right ()
      where
        txResentRequired delegatePublicKey =
          let
            delegatePKH =
              toPlutusKeyHash $
                verificationKeyHash delegatePublicKey
           in
            case decodeNewBidTxOnL2 transaction utxo of
              Just
                ( MkNewBidTxInfo
                    { submitterPKH
                    , isStandingBidInvalidated
                    , newBidDatum
                    }
                  ) ->
                  if submitterPKH == delegatePKH
                    && isStandingBidInvalidated
                    then Just newBidDatum
                    else Nothing
              Nothing -> Nothing
        resentBid actor auctionTerms newBidDatum = do
          let (MkOpenHeadUtxo {standingBidUtxo, collateralUtxo}) = utxoState
          newTx <-
            createNewBidTx
              auctionTerms
              actor
              standingBidUtxo
              collateralUtxo
              newBidDatum
          submitTx newTx
