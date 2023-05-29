# Off-chain architecture and specification

In the Hydra Auction architecture,
commands/requests flow from people (Seller, Bidder, Delegate)
to systems (Frontend/CLI, Cardano node, delegate server, Hydra node) as follows:

```mermaid
flowchart TB
  subgraph Auction App
    seller([Seller])
    bidder([Bidder])
    frontend(Frontend CLI)
  end

  subgraph Hydra-based Auction Hosting
    delegate([Delegate])
    delegateServer(Delegate server)
    hydraNode(Hydra node)
  end

  subgraph Cardano
    cardanoNode(Cardano node)
  end

  admin([Admin])

  admin -- start/stop -----> cardanoNode
  seller -- request --> frontend
  bidder -- request --> frontend
  frontend -- request --> cardanoNode
  frontend -- request --> delegateServer
  delegateServer -- request --> hydraNode
  delegate -- start/stop ---> delegateServer
  delegate -- start/stop ---> hydraNode
  hydraNode -- request --> cardanoNode
```

In the rest of this document,
we will describe each of these systems
and the request types that people can submit to those systems.

# Decisions

* Anyone can ask Delegate server to place auction into Hydra Head.
    * At the same time any Auction has its Hydra Head Id recorded on-chain
    and cannot be placed on any other Hydra Head.

## Off-chain workflow

A typical workflow for an auction should look as follows:

1. The delegates ensure that all daemon services are started
   (Delegate servers, Hydra nodes).
2. Delegates initialize the Hydra Head.
3. The seller announces the auction.
4. **(Bidding start time is reached)**
5. The seller starts the bidding phase.
6. A delegate moves the standing bid to L2.
7. The delegates open the Hydra Head.
8. Bidders may submit new bids on L2.
9. One of the delegates or bidders closes the Hydra Head on L1.
The delegates' Hydra nodes continuously monitor L1 for this closing transaction,
ready to submit contesting transactions if necessary.
10. The seller or one of the bidders or delegates
fans out the standing bid to L1.
11. **(Bidding end time is reached)**
12. The winning bidder may buy the auction lot.
13. **(Voucher expiry time is reached)**
14. If the auction lot has not been bought by the winning bidder,
then the seller reclaims the auction lot
and may claim the winning bidder’s bidder deposit.
15. One of the delegates distributes the auction fees to the delegates.
16. **(Cleanup time is reached)**
17. The seller spends the standing bid utxo and burns the voucher token.

## Services

### Cardano node

The Cardano node is responsible for
broadcasting transactions to the Cardano network.

In the real world, this would correspond to the node,
provided by a light-wallet backend.
Alternatively, for full decentralization, a user could in principle
choose to run their own Cardano node, without a trusted intermediary.

For testing purposes, a local Cardano node cluster (with one node)
can be started using `docker-compose`.

<table><tr><td>

**start.** Start a Cardano node, as a single-node development network (devnet)
or as a node on a known Cardano network (mainnet or testnet).

Command parameters:

- Cardano node config (state directory, tracer, etc.)
- Cardano network type = Devnet | Known network

</td></tr><tr></tr><tr><td>

**stop.** Stop the Cardano node.

</td></tr></table>

### Hydra node

The Hydra node is responsible for broadcasting L2 transactions to the Hydra Head
and participating in the Hydra Head consensus protocol
(including L1 Hydra Head transactions).

The Hydra node API should be only accessible to the delegate server.
It should not be directly accessible to sellers or bidders.

The delegate server may send the following commands to the Hydra node:
`Init`, `Commit`, `NewTx`, `GetUTxO`, `Close`, `Fanout`.

Hydra API reference: https://hydra.family/head-protocol/api-reference

### Delgate server

#### Overall principles

Delegate server works on event-based architecture.
Delegate domain logic state is fully determined by `DelegateState`.
This state is public and broadcasted to all clients on any change.

Delegate does anything only reacting to events.
It can change its state, put transactions on L2 or message client(s).
Possible events are: delegate start, received output from Hydra Node,received client request, current auction stage change.

There is also some technical state, like client connections,
but they do not matter for domain logic.

It is hard to coordinate, which Delegate will request
state change one Hydra Head, so they just do that all at once.
Head should handle that situation correctly.

Clients are connected by WebSockets async event messaging protocol.

#### State machine

`DelegateState` is mainly just reflecting Head state machine.
That way it cand be fully restored by replaing Head outpus
if delegate server fails and all problems of maintaining states
in sync is delegated to Hydra Nodes.
Apart from things specified by Hydra state machine,
`DelegateState` also stores UTxO state (from commited or open head)
in parser domain-specific form.

Fact that `DelegateState` only stores Head state means
that it does not know `AuctionTerms`,
because it cannot be determined from Standing Bid transaction.
That is why any client request should attach `AuctionTerms`,
and delegate server only checks that they do match one used
in standing bid.

There are two exceptions from this scheme:

Any initialized state contains `HeadParams`,
which is known by any delegate from CLI start
and consist of params required by seller to select Head,
like `auctionFeePerDelegate` field.
This works because `DelegateState` is shared by Platform server
to all clients.

There is also a state `AbortRequested AbortReason`,
because it is not determined by Hydra outputs log.
It is used only for storing/communicating debug information,
abusing fact that all `DelegateState` changes are broadcasted to client.

### Requests

* `QueryCurrentDelegateState` - returns `CurrentDelegateState`.
* `CommitStandingBid` - asks to commit standing bid.
   Could only be performed if delegate state is `NotYetOpen`.
* `NewBid` - asks to perform new bid.
  Client gives `StandingBidDatum` for that, which contains
  bid amount and signatures.
  Could only be performed if delegate state is `Open`.
  If another bid was placed on L2 concurenlty, but bid is still valid,
  then delegate server will try to place it any way.

### Generic responses

* `CurrentDelegateState` - delegate state was updated or queried.
* `RequestIgnored IncorrectRequestData` - something wrong
   in request data. For example wrong bid amount or auction terms.
* `RequestIgnored WrongDelegateState` - command cannot be performed
  in current state

### Platform server

#### Overall principles

The purpose of Platform server is to be the way for sellers to find
suitable and available Hydra Head and to 1

Just like Delegate server, Platform server is based on event driven
architecture. It only does something on events.

But unlike Delegate server its logic is not state machine.
It works with data in CQRS-like way
and stores entities in durable and queryable storage.

All entities are reflecting some L1 state.
In theory server could monitor L1 for entities creation/state changes.
But for now it only accepts requests from
and query L1 to check that they are honest.

All entities are public and can be queried.
All queried can be limited by enitity count.

Platform server clients could be both Frontend CLI and Delegate server.

#### Stored entities

* Initialize Hydra Heads:
  stores their current `DelegateState`
  and list of Delegate server addresses.
  L2 standing bid state is not tracked,
  for that client should connect to some Delegate server.
  Could be filtered:
  by current state constuctor (opened, closed, etc),
  by Head ID,
  by including Head Node,
  and by `auctionFeePerDelegate` upper bound (`lte`).
  Ordered by `auctionFeePerDelegate` from lower to higher.
* Announced Auctions: only static `AuctionTerms`.
  Can be filtered by `Head ID` and `AuctionTermsHash`.
  For information if Auction is on L2,
  client shoud query matching for Head entity.
* `BidderApproval`: only stores itself.
  Can be filtered by `AuctionTermsHash` and bidder `PubKeyHash`.

#### Query requests

* `Query EnitityQuery` - query any stored entity
  by available filter fields.
  Returns `QueryResponse [Entity]`
* `SubscribeForBidderApproval AuctionTermsHash`.
  Will send `NewBidderApproval` response for any new `BidderApproval`
  for matching auction.

#### Command requests

* `PutHydraHead` - this should be called by Delegate for Initialized Head.
  Duplicate requests are ignored with `AlreadyKnownHead` response.
  After command success, Platform server connects to Delegate as a client
  to monitor `DelegateState` changes.
* `PutAuction AuctionTerms` - this should be called by seller client
  for announced auction.
  Server checks auction state is actually announced on L1.
* `PutBidderApproval` - this should be called by seller
  Server checks existence of matching auction entity
  and consistency of `BiddingApproval`.

#### Generic responses

* `DuplicateCommand` - if enitity is already known, this is response.
* `WrongCommand NotMatchingL1State` - is returned then L1 does not seem to
  match the one that Command should require.
  For example `PutAuction` was called for auction,
  which is not not nnounced yet.

### Frontend CLI

The frontend CLI is a program that can be run by sellers and bidders.
It provides an interactive prompt
for them to submit their actions to interact with the auction.

<table><tr><td>

**start.** Start a frontend CLI session.

Command parameters:

- User
  (selected from a predefined list of actors)

</td></tr><tr></tr><tr><td>

**stop.** Quit the frontend CLI session.

</td></tr></table>

## API

### Frontend CLI

The `AuctionTerms` for the Auction ID are cached in the auction state directory.

<table><tr><td>

**prepare.** Distribute ADA from the faucet
to all the potential users in the auction (Alice, Bob, etc.)
and provide a freshly minted NFT to the recipient,
so that the NFT can be put up for auction.

This action is intended for testing purposes on local devnets.
It has no effect on mainnet or public testnets.

Request parameters:

- NFT recipient

</td></tr><tr></tr><tr><td>

**showUtxos.** Show the utxos owned by the current user.

**showAllUtxos.** Show the utxos of all users (Alice, Bob, etc.).

Request parameters: none.

</td></tr><tr></tr><tr><td>

**announceAuction.** Used by sellers.

Construct `AuctionTerms` using the request parameters provided
(seller implicitly set to the request submitter),
and submit an L1 transaction to the Cardano node.

From this moment timing of stages begins.

Request parameters:

- Auction ID
- Auction lot asset class
- Hydra Head ID
- Delegates

</td></tr><tr></tr><tr><td>

**createBidDeposit.** Used by bidders to place deposits for auctions,
before the bidding start time.

Request parameters:

- Auction ID
- Deposit amount

This request will return an error if the deposit amount is smaller than
the `minDepositAmount` defined in the auction's terms.

Approved bidders at bidding start time will be selected
from bidders that created sufficient deposits.

</td></tr><tr></tr><tr><td>

**startBiddingL2.** Used by sellers.

Performed after BiddingStart and before BiddingEnd.

Execute a sequence of actions to allow bidding to start on L2.

Request parameters:

- Auction ID
- Delegate Server ID

This endpoint is equivalent to
calling startBiddingL1 and then calling moveStandingBidToL2.

</td></tr><tr></tr><tr><td>

**startBiddingL1.** Used by sellers.

Performed after BiddingStart and before BiddingEnd.

If the request submitter is the seller
corresponding to the `AuctionTerms` of the Auction ID,
then submit an L1 transaction to the Cardano node
to start the bidding for the auction.

Request parameters:

- Auction ID

The list of approved bidders is automatically determined here
via an L1 off-chain query that selects
all bidders who created sufficient deposits for the auction.

</td></tr><tr></tr><tr><td>

**moveStandingBidToL2.** Used by sellers.

Commit the standing bid to the Hydra Head
and coordinate the opening of the Hydra Head.

Request parameters:

- Auction ID
- Delegate server ID

This endpoint performs the following actions:

- Send a request to the chosen delegate server
to commit the standing bid utxo to the Hydra Head.

</td></tr><tr></tr><tr><td>

**newBidL1.** Submit a new bid as an L1 transaction to the Cardano node.

Performed after BiddingStart and before BiddingEnd.

This request is needed for the fallback scenario when
the standing bid is not on L2 because either
it was never moved there
or the Hydra Head closed prematurely.

Normally, all bids should be submitted to L2.

Request parameters:

- Auction ID
- Bid amount

</td></tr><tr></tr><tr><td>

**newBidL2.** Send a request to a given delegate server (chosen by the bidder)
to submit a new bid as an L2 transaction to the Hydra Head.

Performed after BiddingStart and before BiddingEnd.

Request parameters:

- Auction ID
- Bid amount
- Delegate server ID

Response:

- The bidder receives a Hydra Head L1 closing transaction
(with the latest L2 snapshot) signed by the delegate
to whom the newBidL2 request was forwarded,
which he can submit start from (biddingEnd - contestationPeriodDuration).

Cache this post-dated transaction for the bidder.

</td></tr><tr></tr><tr><td>

**bidderBuys.** Used by the winning bidder.

Performed after BiddingEnd and before VoucherExpiry.

Submits a transaction that:
- spends the auction escrow utxo with the `BidderBuys` redeemer
- spends (if available) the bidder's deposit with the `WinningBidder` redeemer
- spends an input from the bidder to provide ADA for the outputs and transaction fees
- sends the bid payment to the seller
- sends the auction lot to the bidder
- sends the total auction fees for delegates to the fee escrow script
- sends the ADA change to the bidder

Request parameters:

- Auction ID

</td></tr><tr></tr><tr><td>

**sellerReclaims.** Used by the seller.

Performed after VoucherExpiry.

Submits a transaction that:
- spends the auction escrow utxo with the `SellerReclaims` redeemer
- spends (if available) the bidder's deposit with the `SellerClaimsDeposit` redeemer
- spends an input from the seller to provide ADA for transaction fees
- sends the auction lot to the seller
- sends the total auction fees for delegates to the fee escrow script
- sends the ADA change to the seller

Request parameters:

- Auction ID

</td></tr><tr></tr><tr><td>

**refundDeposit.** Used by losing bidders.

Performed after BiddingEnd.

Submits a transaction that:
- spends the bidder's deposit
- sends ADA to the bidder

Request parameters:

- Auction ID

</td></tr><tr></tr><tr><td>


**closeHeadByBidder.** Submit the cached post-dated L1 closing transaction
to the Cardano node.

This request is needed for the fallback scenario
when the delegates fail to close the Hydra Head
in time for the contestation period to end
by the bidding end time.
In that scenario, a bidder can use this request
to force the Hydra Head to close, preventing
the Hydra Head from staying open
significantly past the bidding end time.

No request parameters are required.

</td></tr><tr></tr><tr><td>

**fanoutByBidder.** Fan out the standing bid utxo from the Hydra Head,
so that it can be used in L1 transactions.

This request is needed in case the delegates'
hydra nodes fail to fan out the standing bid
after the Hydra Head closes.
Normally, that should happen automatically.

No request parameters are required.

</td></tr><tr></tr><tr><td>

**cleanupStandingBid.** Used by the seller.

Performed after an auction's cleanup time.

Spend the standing bid utxo with the `Cleanup` redeemer
and recover the min 2 ADA that was put into it.

Request parameters:

- Auction ID

</td></tr></table>

### Delegate server

How Delegate server API works:

* Delegate server may have multiple clients which it does not authenticate,
  because all permissions are already enforced on-chain.
* Delegate server works in async event-driven way, the same as Hydra Node.
  Clients could push requests and receive Delegate server responses,
  in async way.
  We name them Requests/Responses, not Inputs/Outputs like Hydra.
  That is because Delegate may have inputs other than Frontend requests.
  Delegate server do broadcast all Hydra events which could be interesting for its clients.

Things which should be done automatically:

* Delegate server should be started when Hydra node already running.
  After start it asks for the initialization of the Hydra Head.
* When one of the delegates makes a commit
  (via Hydra node) with the standing bid UTxO,
  all of the other delegates should make empty commits.
  They should be able to do this by monitoring
  the commit messages on L2.
* The Hydra Head is opened by Hydra node automatically
  as soon as all Hydra nodes commit.
* If the Hydra Head isn't opened before the bidding end time,
  it should be aborted by Delegate server.
* Delegate server closes the Hydra Head at the bidding end time.
  Otherwise, bidders would be able to submit new bids on L2
  past the bidding end time.
* Delegate server submits fee distribution transaction,
  if after voucher expiry fee escrow is present.
* `Fanout` should be triggered when the contestation
  period ends.

<table><tr><td>

Frontend Requests:

**commitStandingBid.** Used by `moveStandingBidToL2` in Frontend CLI.

Request parameters:

* The Utxo that should be commited.
* Serialized `AuctionTerms`.

The `AuctionTerms` should be validated by the delegate
by computing a `CurrencySymbol` of the voucher minting policy
applied to these terms
and comparing it to the voucher token
contained in the utxo to be committed.

**Note:** Implementation for this API request is currently blocked
because Hydra nodes do not support creating transactions
to commit utxos from script addresses to a Hydra Head.
The implementation details of this request depend on
how this feature will be implemented in Hydra.

</td></tr><tr></tr><tr><td>

**newBid.** Construct and submit a `NewBid` transaction on L2,
based on the information provided in the request parameters.

Request parameters:
- Bid datum (including bidder signature)

This request is sent by the Frontend CLI when it receives
a `newBidL2` request from a bidder.

</td></tr></table>
