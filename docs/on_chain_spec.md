# Hydra auction blockchain protocol

The hydra auction blockchain protocol uses
three minting policies,
three non-fungible tokens,
two fungible tokens,
and seven validator scripts.

The auction minting policy atomically mints the
auction escrow,
auction metadata,
and standing bid non-fungible tokens.
The auction state is controlled by the
auction escrow,
auction metadata,
bidder deposit,
fee escrow,
and standing bid validators.

The delegate metadata minting policy mints
delegate metadata fungible token,
which is held under the delegate metadata validator.

The personal oracle minting policy mints
personal oracle fungible token,
which is held under the personal oracle validator.

```mermaid
flowchart
  direction LR

  subgraph RIGHT[ ]
    direction TB
    auctionTerms[[Auction terms]]
    delegateMetadata[Delegate metadata validator]
    delegateMetadataMP{{Delegate metadata MP}}

    auctionEscrow[Auction escrow validator]
    auctionMetadata[Auction metadata validator]
    bidderDeposit[Bidder deposit validator]
    feeEscrow[Fee escrow validator]
    standingBid[Standing bid validator]

    auctionMP{{Auction MP}}

    auctionUtxoNonce([Auction mint input])

    params[[Auction terms + Auction CS]]

    auctionUtxoNonce -.-> auctionMP
    auctionMetadata -.-> auctionMP
    
    auctionMP -.-> params
    auctionTerms  -.-> params

    params -.-> auctionEscrow
    params -.-> bidderDeposit
    params -.-> feeEscrow
    params -.-> standingBid

    standingBid -.-> auctionEscrow
    feeEscrow -.-> auctionEscrow
    auctionEscrow -.-> bidderDeposit
  end

  subgraph LEFT[ ]
    direction TB
    sellerPkh[[Seller Pkh]]
    personalOracleValidator[Personal oracle validator]
    personalOracleMP{{Personal oracle MP}}
    
    sellerPkh -.-> auctionTerms
    sellerPkh -.-> personalOracleMP
    sellerPkh -.-> personalOracleValidator
  end

  classDef default font-size:90%, overflow:visible;
  style LEFT fill:#0000,stroke-width:0px;
  style RIGHT fill:#0000,stroke-width:0px;
```

The auction metadata validator,
delegate metadata validator,
and delegate metadata minting policy
are all unparametrized.

The personal oracle validator
and personal oracle minting policy
are both parametrized by the seller's pubkey hash.

The auction minting policy is parametrized on
the auction metadata validator
and on the utxo reference of an input
to the auction minting transaction.

The other four validators are all parameterized
on the auction terms and the auction currency symbol.

## Announcements, discoverability, and state

All auctions and delegate groups are announced on the L1 blockchain ledger
with all relevant information about them declared
in metadata records held under the non-parametric metadata validators.
This allows any potential participant in the auction protocol
to discover all active auctions and delegate groups by querying the blockchain.

### Delegate group announcement

The purpose of the delegate group announcement is to provide
an official mechanism for a group of delegates to announce
their availability to host auctions on their L2 hydra heads
and provide information about the group.

```haskell
data DelegateInfo = DelegateInfo
  { di'GroupName :: Text
  , di'GroupURL  :: Text
  , di'Delegates :: [PubKeyHash]
  }
```

This announcement transaction mints a delegate metadata token
and outputs a utxo that holds data about the delegate group,
sending it to the delegate metadata validator.

```mermaid
flowchart LR
  tx --> output1([Delegate metadata])
  mint{{Delegate metadata MP}} -- Mint Delegate Metadata --> tx

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

The delegate metadata minting policy ensures that the announcement
is unanimously authorized by the delegates,
while the corresponding validator ensures that the
delegate metadata can only be modified or taken down
by unanimous consent of the delegates.
The delegate metadata contains the pubkey hashes of the delegates,
which is how the two scripts are able to verify delegates' consent.

The output of the delegate group announcement is not
directly referenced or spent by the auction minting policy
or any of the auction validators.
Rather, it exists purely as a mechanism for potential participants
to discover the delegate group via blockchain queries.

To discover all currently existing delegate groups,
query the utxos at delegate metadata validator address,
and then filter out utxos that don't contain
the delegate metadata token.

### Auction announcement

A seller's auction announcement declares the auction metadata
and initializes the auction by minting its state tokens,
placing them along with the auction lot
into the auction escrow.

```mermaid
flowchart LR
  input1([Auction lot from Seller]) --> tx
  mint{{Auction MP}} -- Mint Auction --> tx
  tx --> output1([Auction escrow])
  tx --> output2([Auction metadata])

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

The auction is instantiated by parametrizing its state and metadata tokens,
along with all of its validator scripts,
on the utxo reference of
the input (utxo nonce)
spent to provide the auction lot from the seller
in the announcement transaction.

The auction metadata utxo datum contains
the auction terms,
the auction state token currency symbol,
and all of the auction's validator addresses.

```haskell
data AuctionInfo = AuctionInfo
  { ai'AuctionId     :: CurrencySymbol
  -- ^ The auction is uniquely identified by
  -- the currency symbol of its state tokens.
  , ai'AuctionTerms  :: AuctionTerms
  -- ^ The auction terms fully characterize the
  -- behaviour of the auction.
  , ai'AuctionEscrow :: Address
  , ai'BidderDeposit :: Address
  , ai'FeeEscrow     :: Address
  , ai'StandingBid   :: Address
  }
```

To discover all currently existing auctions,
query the utxos at the auction metadata validator address,
filtering out those with invalid auction terms.
For any auctions of interest,
discover their current L1 state by
querying the utxos at their corresponding validator addresses,
filtering out those that don't contain
the appropriate auction state tokens.

### Auction state and utxo datums

The state of the auction protocol is contained in the
auction escrow, standing bid, and bidder deposits' utxo datums:

```haskell
type AuctionEscrowDatum = AuctionEscrowState

type StandingBidDatum = StandingBidState

type BidderDepositDatum = BidderInfo

type FeeEscrowDatum = ()

data AuctionEscrowState = AuctionAnnounced
                        | BiddingStarted
                        | AuctionConcluded

newtype StandingBidState = StandingBidState
  { standingBidState :: Maybe BidTerms }
```

The auction escrow and standing bid state can change
over the lifecycle of the auction
and they are uniquely identified
by the non-fungible auction state tokens that their utxos hold —
all other utxos under the auction escrow and standing bid
addresses must be ignored as illegitimate.

Bidder deposits are not uniquely identified by any non-fungible tokens,
and bidder information is fixed when its deposit utxo is created.

The location of the standing bid token determines
if and where bids can be placed in the auction:

- If it's at the auction escrow address on L1,
then bids cannot be placed.
- If it's at the the standing bid address on L1,
then bids can be placed on L1.
- Otherwise, it's in a hydra head on L1
and at the standing bid address on L2,
so bids should be placed on L2 via requests sent to the delegates.

### Auction terms

An auction's behaviour is characterized by its **auction terms**,
which are fixed when it is announced.

```haskell
data AuctionTerms = AuctionTerms
  { at'AuctionLot :: AssetClass
  -- ^ NFT being sold in the auction.
  , at'SellerPkh :: PubKeyHash
  -- ^ Seller's pubkey hash, which will receive
  -- the proceeds of the auction (minus fees)
  -- if the auction lot is purchased,
  -- or reclaim the auction lot if it isn't.
  , at'SellerVk  :: BuiltinByteString
  -- ^ Seller's verification key, used to control
  -- which bidders receive authorization to participate in the auction.
  , at'Delegates :: [PubKeyHash]
  -- ^ Group of delegates authorized to run the L2 bidding process.
  , at'BiddingStart :: POSIXTime
  -- ^ Start time of the bidding period.
  , at'BiddingEnd :: POSIXTime
  -- ^ End time of the bidding period.
  , at'PurchaseDeadline :: POSIXTime
  -- ^ Time by which the winning bidder can buy the auction lot.
  -- At and after this time, the winning bidder forfeits its bidder deposit
  -- if the auction lot has not been purchased.
  , at'Cleanup :: POSIXTime
  -- ^ Time at and after  which the remaining utxos in the auction
  -- can be unconditionally cleaned up, returning all tokens
  -- in those utxos to their original owners before the auction.
  , at'AuctionFeePerDelegate :: Integer
  -- ^ Fee portion that each delegate will receieve from
  -- the proceeds of the auction, whether the auction lot
  -- is purchased by a bidder or reclaimed by the seller.
  , at'StartingBid :: Integer
  -- ^ Bids cannot be lower than this number.
  , at'MinimumBidIncrement :: Integer
  -- ^ New bids can only supersede the standing bid if they exceed it
  -- by this increment.
  , at'MinDepositAmount :: Integer
  -- ^ Minimal amount of ADA that the seller requests
  -- each bidder to place as a bidder deposit for the auction.
  -- This is only enforced off-chain at the seller's discretion.
  }
```

Though the auction terms are not validated on-chain,
off-chain observers should consider them
to be valid if the following conditions hold:

```haskell
validAuctionTerms :: AuctionTerms -> Bool
validAuctionTerms auTerms@AuctionTerms {..} =
  at'SellerPkh == hashVKey at'SellerVk &&
  -- The seller pubkey hash corresponds to the seller verification key.
  -- Note: this check only becomes possible on-chain in Plutus V3.
  -- https://github.com/input-output-hk/plutus/pull/5431
  at'BiddingStart < at'BiddingEnd &&
  -- Bidding ends after it the bidding start time.
  at'BiddingEnd < at'PurchaseDeadline &&
  -- The purchase deadline occurs after bidding ends.
  at'PurchaseDeadline < at'Cleanup &&
  -- Cleanup happens after the purchase deadline,
  -- so that the seller can claim the winning bidder's deposit
  -- if the auction lot is not sold
  at'MinimumBidIncrement > 0 &&
  -- New bids must be larger than the standing bid.
  at'AuctionFeePerDelegate > 2_000_000 &&
  -- The auction fee for each delegate must contain
  -- the min 2 ADA for the utxos that will be sent to the delegates
  -- during fee distribution.
  at'StartingBid > totalAuctionFees auTerms &&
  -- The auction fees for all delegates must be covered by the starting bid.
  length at'Delegates > 0 &&
  -- There must be at least one delegate.

totalAuctionFees :: AuctionTerms -> Integer
totalAuctionFees AuctionTerms {..} =
  at'AuctionFeePerDelegate * length at'Delegates
```

Subject to these validity conditions,
the seller can set the auction terms according to the seller's preferences.
The seller should set the `at'Delegates` field
to one of the delegate groups that the seller discovers
by querying the announced delegate groups.

### Bidder deposits

To indicate an interest to participate in an auction,
each bidder should deposit some ADA
under the auction's bidder deposit validator,
identifying the bidder in the bid deposit's utxo datum.

```haskell
data BidderInfo = BidderInfo
  { bi'BidderPkh :: PubKeyHash
  -- ^ Bidder's pubkey hash, which can spend this bidder deposit
  -- to buy the auction lot if a bid placed by bi'BidderVk wins
  -- or reclaim this bid deposit if someone else's bid wins.
  , bi'BidderVk :: PubKeyHash
  -- ^ Bidder's verification, which can authorize bids that allow
  -- the seller at'SellerPkh to claim this bidder deposit
  -- if the bid placed by bi'BidderVk won but the auction lot
  -- wasn't purchased by the deadline.
  }

validBidderInfo :: BidderInfo -> Bool
validBidderInfo BidderInfo {..} =
  bi'BidderPkh == hashVKey bi'BidderVk
  -- The bidder pubkey hash corresponds to the bidder verification key.
  -- Note: this check only becomes possible on-chain in Plutus V3.
  -- https://github.com/input-output-hk/plutus/pull/5431
```

If a bidder places a winning bid in the auction
but does not purchase the auction lot by the deadline,
then the winning bidder's deposit is forfeited to the seller.
This doesn't guarantee that the winning bid will be honored in the auction, but the bid deposit can compensate the seller
for the waste of time caused by the bidder's unserious bid.

In the auction terms, the seller defines
the minimum ADA amount (`at'MinDepositAmount`)
that he expects to see in bidder deposits
to consider allowing those bidders to participate.

### Authorization by seller

In a private auction, the seller controls
who is allowed to place bids in the auction.
To authorize a bidder to participate in the auction,
the seller signs a serialized tuple `(auctionId, bidderVk)`
describing the bidder (via verification key)
and the auction (via currency symbol) that the bidder can
participate in.
This signature can be verified
via the seller's verification key (`at'SellerVk`),
which is included in the auction terms.
Once issued, a bidder-auction signature cannot be revoked by the seller
— the bidder is free to continue using it to play new bids in the auction.

The seller should authorize bidders with sufficient deposits
to participate in the auction,
but has full discretion to grant or withhold authorization
to anyone for any reason.

To inform bidders that they have authorization,
the seller can publish bidder-auction authorization signatures
via a personal oracle.
To publish a new list of bidder-auction authorization signatures
`[(signature, auctionId, bidderVk)]`,
the seller mints a new personal oracle token
and places it under his personal oracle validator
into a utxo containing the list in the datum.
The seller can do this as many times as needed,
publishing different lists of signatures each time.

To discover the bidder-auction authorizations from the seller,
query the seller's personal oracle address
and filter out utxos that don't contain
the seller's personal oracle token.

Overall, this is a completely optional mechanism for the seller to
communicate authorization to bidders for auction participation.
In princple, the auction app could just as easily generate
a QR code that the seller could share on Discord, Twitter,
WhatsApp, etc.

### Bid terms and authorization by bidder

Every bid placed in the auction must identify
the bidder and bid price.
It must also provide signatures proving
that the seller authorized the bidder to participate
and that the bidder authorized the bid.

```haskell
data BidTerms = BidTerms
  { bt'Bidder :: BidderInfo
  -- ^ Bidder that submitted the bid.
  , bt'BidPrice :: Integer
  -- ^ Price that the bidder bid to buy the auction lot.
  , bt'BidderSignature :: BuiltinByteString
  -- ^ Bidder's signature (via bi'BidderVk . bt'Bidder) of the
  -- (ai'AuctionId, bt'BidPrice, bi'BidderPkh) tuple,
  -- authorizing a bid at that price to be placed in the auction
  -- and bi'BidderPkh to buy the auction lot if the bid wins.
  , bt'SellerSignature :: BuiltinByteString
  -- ^ Seller's signature (via at'SellerVk) of the
  -- (ai'AuctionId, bi'BidderVk) tuple,
  -- authorizing the bidder bi'BidderVk to place bids in the auction.
  }
```

Bid terms can be verified on-chain or off-chain as follows:

```haskell
validBidTerms :: AuctionTerms -> CurrencySymbol -> BidTerms -> Bool
validBidTerms AuctionTerms {..} auctionId BidTerms {..}
  | BidderInfo {..} <- bt'Bidder =
  validBidderInfo bt'Bidder &&
  -- The bidder pubkey hash corresponds to the bidder verification key.
  verifyEd25519Signature at'SellerVk
    (sellerSignatureMessage auctionId bi'BidderVk)
    bt'SellerSignature &&
  -- The seller authorized the bidder to participate
  verifyEd25519Signature bi'BidderVk
    (bidderSignatureMessage auctionId bt'BidPrice bi'bidderPkh)
    bt'BidderSignature
  -- The bidder authorized the bid

bidderSignatureMessage
  :: CurrencySymbol
  -> Integer
  -> PubKeyHash
  -> BuiltinByteString
bidderSignatureMessage auctionId bidPrice bidderPkh =
  toByteString auctionId <>
  toByteString bidderPkh <>
  toByteString bidPrice

sellerSignatureMessage
  :: CurrencySymbol
  -> BuiltinByteString
  -> BuiltinByteString
sellerSignatureMessage auctionId bidderVk =
  toByteString auctionId <>
  bidderVk
```

## Minting policies

### Auction minting policy

The purpose of the auction state and metadata tokens is
to provide an unbroken path of provenance
from the auction lot input of the auction announcement transaction
(which instantiates the auction)
to the unique auction metadata,
auction escrow,
and standing bid utxos of the auction.
Furthermore, the auction minting policy requires
all of these tokens to be minted together and burned together;
since the two state tokens are continuously held
under auction protocol validators for the entire lifecycle of the auction,
this also means that the minting policy ensures their continued existence
for that entire lifecycle.

```haskell
data AuctionPolicyRedeemer = MintAuction
                           | BurnAuction
```

The token names of this minting policy are as follows:

```haskell
-- Auction state token, identifying the true auction escrow.
auctionTN :: TokenName
auctionTN = "AUCTION"

-- Auction metadata token, identifying the true auction metadata.
auctionMetadataTN :: TokenName
auctionMetadataTN = "AUCTION_METADATA"

-- Standing bid token, identifying the true standing bid.
standingBidTN :: TokenName
standingBidTN = "STANDING_BID"
```

Under the `MintAuction` redeemer, we enforce that:

- The utxo nonce parameter of the minting policy refers to a transaction input.
- There are three tokens minted.
They have the minting policy's own currency symbol
and the token names
`auctionTN` (auction state token),
`auctionMetadataTN` (auction metadata token),
and `standingBidTN` (standing bid token).
- The auction metadata token is sent to the auction metadata validator.

```mermaid
flowchart LR
  input1([Utxo nonce]) --> tx
  mint{{Auction MP}} -- Mint Auction --> tx
  tx --> auctionMetadata([Auction metadata])

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

Under the `BurnAuction` redeemer, we enforce that:

- There are three tokens burned.
They have the minting policy's own currency symbol
and the token names
`auctionTN` (auction state token),
`auctionMetadataTN` (auction metadata token),
and `standingBidTN` (standing bid token).

```mermaid
flowchart LR
  tx -- Burn Auction --> mint{{Auction MP}}

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

Note that the auction minting policy doesn't validate the auction terms
or ensure that the auction state tokens are sent
to the appropriate auction protocol validators.

### Delegate metadata minting policy

The purpose of this minting policy is to verify that
a `DelegateInfo` datum posted to the delegate metadata validator
has unanimous consent from all delegates that it declares.

```haskell
data DelegateMetadataPolicyRedeemer = MintDelegateMetadata
                                    | BurnDelegateMetadata
```

The token names of this policy are as follows:

```haskell
delegateMetadataTN :: TokenName
delegateMetadataTN = "DELEGATE_METADATA"
```

Under the `MintDelegateMetadata` redeemer, we enforce that:

- There is one token minted.
It has the minting policy's own currency symbol
and the token name `delegateMetadataTN` (delegate metadata token).
- There is one output sent to the delegate metadata validator,
containing the minted token.
Its `DelegateInfo` datum satisfies the `validDelegateInfo` conditions.
- The transaction is signed by all of the pubkey hashes
indicated in the `di'Delegates` field of the `DelegateInfo` output datum.

```mermaid
flowchart LR
  mint{{Delegate Metadata MP}} -- Mint Delegate Metadata --> tx
  tx --> delegateMetadata([Delegate metadata])

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

Under the `BurnDelegateMetadata` redeemer, we enforce that:

- There is one token burned.
It has the minting policy's own currency symbol
and the token name `delegateMetadataTN` (delegate metadata token).
- There is one input from the delegate metadata validator,
containing the burned token and a `DelegateInfo` datum.
- The transaction is signed by all of the pubkey hashes
indicated in the `di'Delegates` field of the `DelegateInfo` input datum.

```mermaid
flowchart LR
  delegateMetadata([Delegate metadata]) --> tx
  tx -- Burn Delegate Metadata --> mint{{Delegate Metadata MP}}

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

### Personal oracle minting policy

Mint or burn if the seller signs, whenever desired.

## Auction state validators

### Auction escrow validator

The purpose of the auction escrow validator is
to hold the auction lot after the auction announcement,
until either the winning bidder buys it
or the seller reclaims it.

```haskell
data AuctionEscrowRedeemer = StartBidding
                           | SellerReclaims
                           | BidderBuys
                           | CleanupAuction
```

This validator is parametrized by the auction terms
and the currency symbol of the auction state tokens.
All references to "the auction state token"
"the standing bid token",
or "the auction metadata token" below
imply tokens with this currency symbol.

When the auction is announced,
the seller should initialize the auction escrow datum
as `AuctionAnnounced :: AuctionEscrowState`.

Under the `StartBidding` redeemer, we enforce that:

- There is one input spent from the auction escrow validator,
containing the auction lot,
the auction state token,
and the standing bid token.
Its `AuctionEscrowState` datum is `AuctionAnnounced`.
- There is one output sent to the auction escrow validator,
containing the auction lot and the auction state token.
Its `AuctionEscrowState` datum is `BiddingStarted`.
- There is one output sent to the standing bid validator,
containing the standing bid token.
Its `StandingBidState` datum is `StandingBidState Nothing`.
- The transaction validity interval starts
at the bidding start time
and ends before the bidding end time.
- The transaction is signed by the seller `at'SellerPkh`.
- No tokens are minted or burned.

```mermaid
flowchart LR
  input1([Auction escrow]) -- Start Bidding --> tx
  tx --> output1([Auction escrow])
  tx --> output2([Standing bid])

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

Under the `SellerReclaims` redeemer, we enforce that:

- There is one input spent from the auction escrow validator,
containing the auction lot and the auction state token.
Its `AuctionEscrowState` datum is `BiddingStarted`.
- There is one input spent from the standing bid validator,
containing the standing bid token.
- There is one output sent to the auction escrow validator,
containing the auction state and standing bid tokens.
Its `AuctionEscrowState` datum is `AuctionConcluded`.
- There is one output sent to the seller,
containing the auction lot.
- There is one output sent to the fee escrow validator,
containing the total auction fees
that will be distributed to the delegates,
as calculated by `totalAuctionFees`.
- The transaction validity interval
starts at the purchase deadline time.
- No tokens are minted or burned.

```mermaid
flowchart LR
  input1([Auction escrow]) -- Seller reclaims --> tx
  input2([Standing bid]) --> tx
  tx --> output1([Auction escrow])
  tx --> output2([Auction lot to Seller])
  tx --> output3([Fee escrow])

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

Under the `BidderBuys` redeemer, we enforce that:

- There is one input spent from the auction escrow validator,
containing the auction lot and the auction state token.
Its `AuctionEscrowState` datum is `BiddingStarted`.
- There is one input spent from the standing bid validator,
containing the standing bid token.
Its `StandingBidState` datum contains a bid with `BidTerms`
defining the bidder and the bid price.
- There is one output sent to the auction escrow validator,
containing the auction state and standing bid tokens.
Its `AuctionEscrowState` datum is `AuctionConcluded`.
- There is one output sent to a buyer,
containing the auction lot.
- There is one output sent to the seller,
containing an ADA payment to the seller.
- There is one output sent to the fee escrow validator,
containing the total auction fees
that will be distributed to the delegates,
as calculated by `totalAuctionFees`.
- The conditions in `validBuyer` and `validSellerPayment`
are satisfied when applied to the relevant arguments.
- The transaction validity interval
starts at the bidding end time
and ends before the purchase deadline time.
- No tokens are minted or burned.

```mermaid
flowchart LR
  input1([Auction escrow]) -- Bidder Buys --> tx
  input2([Standing bid]) --> tx
  tx --> output1([Auction escrow])
  tx --> output2([Auction lot to Buyer])
  tx --> output3([Payment to Seller])
  tx --> output4([Fee escrow])

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

The conditions for a valid buyer and a valid payment to the seller are:

```haskell
validBuyer
  :: AuctionTerms
  -> CurrencySymbol
  -> StandingBidState
  -> PubKeyHash
  -> Bool
validBuyer auTerms@AuctionTerms{..} auctionId StandingBidState{..} buyer
  | Just bidTerms@BidTerms{..} <- standingBidState
  , BidderInfo {..} <- bt'Bidder =
      buyer == bi'BidderPkh &&
      validBidTerms auTerms auctionId bidTerms
  | otherwise = False

validPaymentToSeller
  :: AuctionTerms
  -> StandingBidState
  -> Int
  -> Bool
validPaymentToSeller auTerms StandingBidState {..} payment
  | Just BidTerms{..} <- standingBidState =
      payment >= bt'BidPrice - totalAuctionFees auTerms
  | otherwise = False
```

Under the `CleanupAuction` redeemer, we enforce that:

- There is one input spent from the auction escrow validator,
containing the auction state and standing bid tokens.
Its `AuctionEscrowState` datum is `AuctionConcluded`.
- There are three tokens burned:
the auction state token,
the standing bid token,
and the auction metadata token.
- The transaction validity interval
starts at the cleanup time.

```mermaid
flowchart LR
  input1([Auction escrow]) -- Cleanup Auction --> tx
  tx --> output1{{Auction MP}}

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```


### Standing bid validator

The purpose of the standing bid validator is to
manage the state transitions of the standing bid
and to continuously maintain control over
the standing bid token
until it its spent when either
the seller reclaims the auction lot
or the winning bidder buys it.

```haskell
data StandingBidRedeemer = MoveToHydra
                         | NewBid
                         | ConcludeAuction
```

Under the `MoveToHydra` redeemer, we enforce that:

- There is one input from the standing bid validator,
containing the standing bid token.
- There is one input from the Hydra Head initial validator
($\nu_\textrm{initial}$),
containing some Hydra Head participation token.
The redeemer provided to this input
mentions the standing bid input by utxo reference.
- There is one output sent to the Hydra Head commit validator
($\nu_\textrm{commit}$),
containing the Hydra Head participation token
and the standing bid token.
- The transaction is signed by all of the delegates `at'Delegates`.
- No tokens are minted or burned.

```mermaid
flowchart LR
  input1([Standing bid]) -- Move to Hydra --> tx
  input2([Hydra Head initial]) --> tx
  tx --> output1([Hydra Head commit])
  tx

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

Under the `NewBid` redeemer, we enforce that:

- There is one input from the standing bid validator,
containing the standing bid token.
Its `StandingBidState` datum is the old standing bid state.
- There is one output to the standing bid validator,
containing the standing bid token.
Its `StandingBidState` datum is the new standing bid state.
- The conditions in `validNewBid` are satisfied
when applied to the relevant arguments.
- The transaction validity range ends before the bidding end time.
- No tokens are minted or burned.

```mermaid
flowchart LR
  input1([Standing bid]) -- New Bid --> tx
  tx --> output1([Standing bid])

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

The conditions for a valid transition in the standing bid state are:

```haskell
validNewBid
  :: AuctionTerms
  -> CurrencySymbol
  -> StandingBidState
  -> StandingBidState
  -> Bool
validNewBid auTerms@AuctionTerms {..} auctionId old new =
  case standingBidState new of
    Nothing ->
      False
    Just newTerms ->
      validNewTerms auTerms auctionId newTerms &&
      case standingBidState old of
        Nothing ->
          validStartingBid auTerms newTerms
        Just oldTerms ->
          validBidIncrement auTerms oldTerms newTerms

validStartingBid :: AuctionTerms -> BidTerms -> Bool
validStartingBid AuctionTerms {..} BidTerms {..} =
  at'StartingBid <= bt'BidPrice

validBidIncrement :: AuctionTerms -> BidTerms -> BidTerms -> Bool
validBidIncrement AuctionTerms {..} old new =
  bt'BidPrice old + at'MinimumBidIncrement <= bt'BidPrice new
```

Under the `ConcludeAuction` redeemer, we enforce that:

- There is one input from the standing bid validator,
containing the standing bid token.
- There is one input from the auction escrow validator,
containing the auction state token.

```mermaid
flowchart LR
  input1([Standing bid]) -- Conclude Auction --> tx
  input2([Auction Escrow]) --> tx

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

### Bidder deposit validator

[TODO] [Avoid double-satisfaction with escrow validator payouts]

The purpose of the bidder deposit validator is
to hold a bidder's ADA deposit for an auction until either:

- The bidder wins and uses the deposit to buy the auction lot.
- The seller claims the deposit
because the bidder won the auction
but did not purchase the auction lot by the deadline.
- The bidder reclaims the deposit
because another bidder won the auction.
- The bidder reclaims the deposit
because the auction has already concluded.

```haskell
data BidderDepositRedeemer = DepositUsedByWinner
                           | DepositClaimedBySeller
                           | DepositReclaimedByLoser
                           | DepositReclaimedAuctionConcluded
                           | DepositCleanup
```

Under the `DepositUsedByWinner` redeemer, we enforce that:

- There is one input spent from the bid deposit validator.
Its `BidderInfo` datum defines the bidder.
- There is one input spent from the auction escrow validator,
containing the auction state token.
- Either the transaction is signed by the bidder who made the deposit or
there is one ouptut sent to that bidder that contains the bid deposit ADA.
- No tokens are minted or burned.

```mermaid
flowchart LR
  input1([Bid deposit]) -- Deposit used by winner --> tx
  input2([Auction escrow]) --> tx
  tx --> output1([Bid deposit to bidder])

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

Under the `DepositClaimedBySeller` redeemer, we enforce that:

- There is one input spent from the bid deposit validator.
Its `BidderInfo` datum defines the bidder who made the deposit.
- There is one input spent from the auction escrow validator,
containing the auction state token.
- There is one input spent from the standing bid validator,
containing the standing bid token.
Its `StandingBidState` datum defines the bidder who placed the bid,
if any.
- Either the transaction is signed by the seller or
there is one output sent to the seller that contains the bid deposit ADA.
- The bidder verification key matches between
the bidder deposit and the standing bid.
- The transaction validity interval starts at the purchase deadline.
- No tokens are minted or burned.

```mermaid
flowchart LR
  input1([Bid deposit]) -- Deposit Claimed by Seller --> tx
  input2([Auction escrow]) --> tx
  input3([Standing bid]) --> tx
  tx --> output1([Bid deposit to seller])
  tx

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

Under the `DepositReclaimedByLoser` redeemer:

- There is one input spent from the bid deposit validator.
Its `BidderInfo` datum defines the bidder who made the deposit.
- There is one reference input from the standing bid validator,
containing the standing bid token.
Its `StandingBidState` datum defines the bidder who placed the bid,
if any.
- Either the transaction is signed by the bidder who made the deposit or there is one output sent to that bidder that contains the bid deposit ADA.
- The bidder verification key _doesn't match_ between
the bidder deposit and the standing bid.
- The transaction validity interval starts at the bidding end time.
- No tokens are minted or burned.

```mermaid
flowchart LR
  input1([Bid deposit]) -- Deposit Reclaimed by Loser --> tx
  input2([Standing bid]):::reference --> tx
  tx --> output1([Bid deposit to bidder])
  tx

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

Under the `DepositReclaimedAuctionConcluded` redeemer, we enforce that:

- There is one input spent from the bid deposit validator.
Its `BidderInfo` datum defines the bidder who made the deposit.
- There is one reference input from the standing bid validator,
containing both the auction state and standing bid tokens.
Its `AuctionEscrowState` datum is `AuctionConcluded`.
- There is one ouptut sent back to the bidder who made the deposit,
containing the bid deposit ADA.
- No tokens are minted or burned.

```mermaid
flowchart LR
  input1([Bid deposit]) -- Deposit Reclaimed Auction Concluded --> tx
  input2([Auction escrow]):::reference --> tx
  tx --> output1([Bid deposit to bidder])
  tx

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

Under the `DepositCleanup` redeemer, we enforce that:

- There is one input spent from the bid deposit validator.
Its `BidderInfo` datum defines the bidder who made the deposit.
- Either the transaction is signed by the bidder who made the deposit or
there is one ouptut sent to that bidder that contains the bid deposit ADA.
- The transaction validity time starts after the cleanup time.
- No tokens are minted or burned.

```mermaid
flowchart LR
  input1([Bid deposit]) -- Deposit Cleanup --> tx
  tx --> output1([Bid deposit to bidder])
  tx

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

### Fee escrow validator

The purpose of this validator is to distribute the total auction fees
evenly among the delegates, after deducting the transaction fee.

Under the `DistributeFees` redeemer, we enforce that:

- There is one input spent from the fee escrow validator.
- There is at least one output per delegate such that
the ada contained in that output is at least `at'AuctionFeePerDelegate`
- No tokens are minted or burned.

```mermaid
flowchart LR
  input1([Fee escrow]) -- Distribute Fees --> tx
  tx --> output1([Auction fee portion to delegate 1])
  tx --> output2([Auction fee portion to delegate 2])
  tx --> output3([Auction fee portion to delegate 3])

  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

The fee distribution to delegates must satisfy the following conditions:

To keep things simple in this design, we require
the number of delegates in an auction to be small enough
that distributing their respective portions of the auction fee
can be done in a single transaction.
Later on, this can be generalized in a straightforward way
to accommodate incremental fee distribution to a larger number of delegates.

## Metadata and oracle validators

### Auction metadata validator

The purpose of this validator is to ensure that
the auction metadata remains unmodified and
the auction metadata token remains under its control
until it is burned (together with the auction state tokens).

```haskell
data AuctionMetadataRedeemer = RemoveAuction
```

Under the `RemoveAuction` redeemer, we enforce that:

- There is one input from the auction metadata validator,
containing one auction metadata token.
- There are three tokens burned:
the auction state token,
the standing bid token,
and the auction metadata token.

```mermaid
flowchart LR
  input1([Auction metadata]) -- Remove Auction --> tx
  tx --> mint{{Auction MP}}
  
  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```
### Delegate metadata validator

The purpose of this validator is to ensure that
the delegate metadata continuously stays under its control
until it is burned (with unanimous consent of the delegates),
and that the delegate metadata is only modified
with unanimous consent of the delegates.

```haskell
data DelegateMetadataRedeemer = ModifyDelegateMetadata
                              | RemoveDelegateMetadata
```

Under the `ModifyDelegateMetadata` redeemer, we enforce that:

- There is one input from the delegate metadata validator,
containing one delegate metadata token and a `DelegateInfo` datum.
- There is one output to the delegate metadata validator,
containing one delegate metadata token and a `DelegateInfo` datum.
- No tokens are minted or burned.
- The transaction is signed by all of the pubkey hashes
indicated in the `di'Delegates` field of the `DelegateInfo` input datum.

```mermaid
flowchart LR
  input1([Delegate metadata]) -- Modify Delegate Metadata --> tx
  tx --> auctionMetadata([Delegate metadata])
  
  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

Under the `RemoveDelegateMetadata` redeemer, we enforce that:

- There is one input from the delegate metadata validator,
containing one delegate metadata token.
- There is one token burned — the delegate metadata token.

```mermaid
flowchart LR
  input1([Delegate metadata]) -- Remove Delegate Metadata --> tx
  tx --> mint{{Delegate metadata MP}}
  
  classDef default font-size:90%, overflow:visible;
  classDef reference stroke-dasharray: 5 5;
```

### Personal oracle validator

TLDR utxos under this validator can be spent by seller, whenever desired.
